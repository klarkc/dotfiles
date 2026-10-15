#!/usr/bin/env python3
"""Local, dependency-free metasearch router with a SearXNG-compatible JSON API.

There is no local API quota. Public engines may still block or rate-limit queries.
"""

import argparse
import base64
import html
from html.parser import HTMLParser
import json
import os
import re
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.error import HTTPError, URLError
from urllib.parse import parse_qs, quote, unquote, urlencode, urlparse
from urllib.request import Request, urlopen

HOST = '127.0.0.1'
PORT = 8979
TIMEOUT = 6
ORDER = ('duckduckgo', 'bing', 'wikipedia', 'searxng')
UA = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125 Safari/537.36'


def clean(value, limit=500):
    return ' '.join(html.unescape(re.sub(r'<[^>]+>', ' ', str(value))).split())[:limit]


def normalize(hits, name, limit=8):
    result, seen = [], set()
    for hit in hits:
        url = html.unescape(str(hit.get('url', '')).strip())
        parsed = urlparse(url)
        if parsed.hostname and parsed.hostname.endswith('bing.com') and parsed.path.startswith('/ck/'):
            target = parse_qs(parsed.query).get('u', [''])[0]
            if target.startswith('a1'):
                try:
                    url = base64.urlsafe_b64decode(target[2:] + '===').decode('utf-8')
                    parsed = urlparse(url)
                except (ValueError, UnicodeError):
                    continue
        if parsed.scheme not in ('http', 'https') or not parsed.netloc or url in seen:
            continue
        title = clean(hit.get('title', ''), 180)
        if len(title) < 3:
            continue
        seen.add(url)
        result.append({'title': title, 'url': url, 'content': clean(hit.get('content', '')), 'engine': name})
        if len(result) >= limit:
            break
    return result


class SearchHtml(HTMLParser):
    def __init__(self, engine):
        super().__init__(convert_charrefs=True)
        self.engine = engine
        self.items = []
        self.current = None
        self.depth = 0
        self.in_title = 0
        self.in_snippet = 0
        self.buffer = []

    def handle_starttag(self, tag, attrs):
        attr = dict(attrs)
        classes = attr.get('class', '').split()
        if self.engine == 'bing':
            if tag == 'li' and 'b_algo' in classes and self.current is None:
                self.current = {'title': '', 'url': '', 'content': ''}
                self.depth = 1
            elif self.current is not None:
                self.depth += 1
                if tag == 'h2':
                    self.in_title = self.depth
                if tag == 'a' and self.in_title and not self.current['url']:
                    self.current['url'] = attr.get('href', '')
                    self.buffer = []
                if tag == 'p' and not self.current['content'] and not self.in_snippet:
                    self.in_snippet = self.depth
                    self.buffer = []
        else:
            if tag == 'a' and 'result__a' in classes:
                self.current = {'title': '', 'url': attr.get('href', ''), 'content': ''}
                self.in_title = 1
                self.buffer = []
            if tag in ('a','div') and 'result__snippet' in classes:
                self.in_snippet = 1
                self.buffer = []

    def handle_data(self, data):
        if self.in_title or self.in_snippet:
            self.buffer.append(data)

    def handle_endtag(self, tag):
        if self.engine == 'bing' and self.current is not None:
            if tag == 'a' and self.in_title and not self.current['title']:
                self.current['title'] = ' '.join(self.buffer)
                self.buffer = []
            if tag == 'p' and self.in_snippet:
                self.current['content'] = ' '.join(self.buffer)
                self.in_snippet = 0
                self.buffer = []
            if tag == 'h2':
                self.in_title = 0
            self.depth -= 1
            if self.depth <= 0:
                if self.current['title'] and self.current['url']:
                    self.items.append(self.current)
                self.current = None
                self.depth = 0
        elif self.engine == 'duckduckgo':
            if tag == 'a' and self.in_title:
                self.current['title'] = ' '.join(self.buffer)
                self.items.append(self.current)
                self.current = None
                self.in_title = 0
                self.buffer = []
            elif tag in ('a', 'div') and self.in_snippet:
                if self.items:
                    self.items[-1]['content'] = ' '.join(self.buffer)
                self.in_snippet = 0
                self.buffer = []


def request_json(url, *, data=None, headers=None):
    req = Request(url, data=json.dumps(data).encode() if data is not None else None,
                  headers={'User-Agent': UA, 'Accept': 'application/json', **(headers or {})})
    return json.loads(urlopen(req, timeout=TIMEOUT).read(750_000))


def request_html(url):
    req = Request(url, headers={'User-Agent': UA, 'Accept-Language': 'en-US,en;q=0.9'})
    return urlopen(req, timeout=TIMEOUT).read(1_000_000).decode('utf-8', 'replace')


def provider(name, query):
    if name == 'searxng':
        base = os.environ.get('SEARCH_ROUTER_SEARXNG_URL', 'http://127.0.0.1:8980').rstrip('/')
        parsed = urlparse(base)
        if parsed.scheme != 'http' or parsed.hostname not in ('127.0.0.1', 'localhost', '::1') or parsed.port in (None, PORT):
            raise ValueError('SearXNG fallback must use a distinct localhost HTTP port')
        data = request_json(base + '/search?' + urlencode({'q': query, 'format': 'json'}))
        return [{'title': x.get('title'), 'url': x.get('url'), 'content': x.get('content', '')} for x in data.get('results', [])]
    if name == 'bing':
        parser = SearchHtml('bing')
        parser.feed(request_html('https://www.bing.com/search?' + urlencode({'q': query, 'count': 8})))
        return parser.items
    if name == 'duckduckgo':
        parser = SearchHtml('duckduckgo')
        parser.feed(request_html('https://html.duckduckgo.com/html/?' + urlencode({'q': query})))
        for result in parser.items:
            url = result['url']
            if url.startswith('//'):
                url = 'https:' + url
            parsed = urlparse(url)
            result['url'] = parse_qs(parsed.query).get('uddg', [url])[0] if parsed.hostname and parsed.hostname.endswith('duckduckgo.com') else url
        return parser.items
    if name == 'wikipedia':
        data = request_json('https://en.wikipedia.org/w/api.php?' + urlencode({'action': 'query', 'list': 'search', 'srsearch': query, 'srlimit': 8, 'format': 'json'}))
        return [{'title': x.get('title'), 'url': 'https://en.wikipedia.org/wiki/' + quote(x['title'].replace(' ', '_')), 'content': x.get('snippet', '')} for x in data.get('query', {}).get('search', [])]
    raise ValueError('Unknown provider')


def site_domains(query):
    """Extract site: domain constraints to enforce locally across fallbacks.

    Public search engines may ignore site: when rate-limited or when their
    HTML endpoint rewrites the query. Filtering returned URLs protects the
    caller from unrelated matches from a fallback provider.
    """
    return tuple(dict.fromkeys(
        domain.strip('.').lower()
        for domain in re.findall(r'(?<!\\S)site:(?:https?://)?([A-Za-z0-9.-]+)', query, re.I)
        if domain.strip('.')
    ))


def matches_site(url, domains):
    host = urlparse(url).hostname
    return bool(host and any(
        host == domain or host.endswith('.' + domain)
        for domain in domains
    ))


class Router:
    def __init__(self, order=None, ttl=600, clock=None):
        names = order if order is not None else os.environ.get('SEARCH_ROUTER_PROVIDERS', ','.join(ORDER)).split(',')
        # The local SearXNG engine always runs LAST, even for custom priority overrides.
        public = tuple(dict.fromkeys(name.strip() for name in names if name.strip() in ORDER and name.strip() != 'searxng'))
        self.order = public + ('searxng',)
        self.clock = clock or time.monotonic
        self.ttl = ttl
        self.cache = {}
        self.cooldown = {}
        self.lock = threading.Lock()

    def search(self, query):
        query = ' '.join(query.split())
        if not 2 <= len(query) <= 256:
            return 400, {'error': 'query must contain 2-256 characters', 'results': []}
        now = self.clock()
        with self.lock:
            cached = self.cache.get(query)
            if cached and cached[0] > now:
                return 200, {**cached[1], 'cached': True}
        failures = []
        sites = site_domains(query)
        for name in self.order:
            with self.lock:
                if self.cooldown.get(name, 0) > self.clock():
                    failures.append({'provider': name, 'status': 'cooldown'})
                    continue
            try:
                hits = provider(name, query)
                if hits is None:
                    continue
                # Filter *after* normalization (which unwraps Bing redirects),
                # but inspect more candidates for site-scoped searches so a
                # valid result is not hidden behind eight unrelated matches.
                normalized = normalize(hits, name, limit=40 if sites else 8)
                results = [hit for hit in normalized if matches_site(hit['url'], sites)][:8] if sites else normalized
                if not results:
                    failures.append({'provider': name, 'status': 'site_mismatch' if sites and normalized else 'empty'})
                    continue
                payload = {'query': query, 'provider': name, 'results': results, 'failed_providers': failures, 'cached': False}
                with self.lock:
                    self.cache[query] = (self.clock() + self.ttl, payload)
                    if len(self.cache) > 256:
                        for key in list(self.cache)[:64]:
                            del self.cache[key]
                return 200, payload
            except HTTPError as error:
                failures.append({'provider': name, 'status': f'http_{error.code}'})
                if error.code in (403, 429, 503):
                    with self.lock:
                        self.cooldown[name] = self.clock() + (120 if error.code != 429 else 300)
                error.close()
            except (OSError, URLError, ValueError, json.JSONDecodeError) as error:
                failures.append({'provider': name, 'status': type(error).__name__})
            except Exception as error:
                failures.append({'provider': name, 'status': type(error).__name__})
        return 503, {'query': query, 'results': [], 'failed_providers': failures, 'error': 'All search providers failed or returned no matches'}


def handler_factory(router):
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            path = urlparse(self.path)
            if path.path == '/health':
                self.respond(200, {'status': 'ok', 'providers': list(router.order)})
                return
            if path.path != '/search':
                self.respond(404, {'error': 'Not found'})
                return
            params = parse_qs(path.query)
            if params.get('format', ['json'])[0] != 'json' or params.get('pageno', ['1'])[0] != '1':
                self.respond(400, {'error': 'Only JSON and first page supported', 'results': []})
                return
            status, payload = router.search(params.get('q', [''])[0])
            self.respond(status, payload)

        def respond(self, status, payload):
            body = json.dumps(payload, ensure_ascii=False).encode('utf-8')
            self.send_response(status)
            self.send_header('Content-Type', 'application/json; charset=utf-8')
            self.send_header('Content-Length', str(len(body)))
            self.send_header('Cache-Control', 'no-store')
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, fmt, *args):
            # Do not log queries: they can include private project text.
            return
    return Handler


def main():
    parser = argparse.ArgumentParser(description='localhost-only web search router')
    parser.add_argument('command', choices=('serve', 'search'))
    parser.add_argument('query', nargs='?')
    args = parser.parse_args()
    engine = Router()
    if args.command == 'search':
        status, payload = engine.search(args.query or '')
        print(json.dumps(payload, ensure_ascii=False))
        return 0 if status == 200 else 1
    server = ThreadingHTTPServer((HOST, PORT), handler_factory(engine))
    print(f'web search router at http://{HOST}:{PORT}', file=sys.stderr, flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
    return 0


if __name__ == '__main__':
    sys.exit(main())
