import base64
import json
from http.server import ThreadingHTTPServer
import threading
import unittest
from unittest.mock import patch
from urllib.error import HTTPError
from urllib.request import urlopen

import router


class RouterTest(unittest.TestCase):
    def test_free_provider_success(self):
        engine = router.Router(order=['duckduckgo'])
        with patch.object(router, 'provider', return_value=[{'title': 'PostGIS docs', 'url': 'https://postgis.net/docs/', 'content': 'GIS reference'}]):
            status, payload = engine.search('PostGIS docs')
        self.assertEqual(status, 200)
        self.assertEqual(payload['provider'], 'duckduckgo')
        self.assertEqual(payload['results'][0]['engine'], 'duckduckgo')

    def test_fallback_and_cooldown_on_429(self):
        engine = router.Router(order=['duckduckgo', 'bing'], ttl=0)
        def sources(provider, _query):
            if provider == 'duckduckgo':
                raise HTTPError('https://example.org', 429, 'Too many', None, None)
            return [{'title': 'PostGIS homepage', 'url': 'https://postgis.net/', 'content': ''}]
        with patch.object(router, 'provider', side_effect=sources) as mock:
            status, payload = engine.search('GIS features')
            status2, payload2 = engine.search('GIS second')
        self.assertEqual((status, status2), (200, 200))
        self.assertEqual(payload['provider'], 'bing')
        self.assertEqual(payload['failed_providers'][0]['status'], 'http_429')
        self.assertEqual(payload2['failed_providers'][0]['status'], 'cooldown')
        self.assertEqual(mock.call_count, 3)

    def test_provider_order_is_keyless_and_local_last(self):
        self.assertEqual(router.ORDER, ('duckduckgo', 'bing', 'wikipedia', 'searxng'))
        self.assertEqual(router.Router(order=['searxng', 'bing', 'duckduckgo', 'bing']).order,
                         ('bing', 'duckduckgo', 'searxng'))
        self.assertEqual(router.Router(order=['wikipedia']).order, ('wikipedia', 'searxng'))

    def test_no_api_keys_ever_required(self):
        with patch.dict('os.environ', {'TAVILY_API_KEY': 'unused', 'BRAVE_SEARCH_API_KEY': 'unused', 'EXA_API_KEY': 'unused'}):
            self.assertNotIn('tavily', router.Router().order)
            self.assertNotIn('brave', router.Router().order)
            self.assertNotIn('exa', router.Router().order)

    def test_local_searxng_is_rejected_if_pointing_to_router(self):
        with patch.dict('os.environ', {'SEARCH_ROUTER_SEARXNG_URL': 'http://127.0.0.1:8979'}):
            with self.assertRaisesRegex(ValueError, 'distinct localhost'):
                router.provider('searxng', 'GIS docs')

    def test_local_endpoint_cannot_target_public_host(self):
        with patch.dict('os.environ', {'SEARCH_ROUTER_SEARXNG_URL': 'https://example.org'}):
            with self.assertRaisesRegex(ValueError, 'distinct localhost'):
                router.provider('searxng', 'GIS docs')

    def test_all_public_engines_fail_then_uses_local(self):
        engine = router.Router(order=['duckduckgo','bing','wikipedia'])
        trace = []
        def sources(name, query):
            trace.append(name)
            if name == 'searxng':
                return [{'title': 'Fallback docs', 'url': 'https://postgis.net/docs/', 'content': 'Local search'}]
            return []
        with patch.object(router,'provider',side_effect=sources):
            status, payload = engine.search('local last fallback query')
        self.assertEqual(status,200)
        self.assertEqual(trace,['duckduckgo','bing','wikipedia','searxng'])
        self.assertEqual(payload['provider'],'searxng')

    def test_site_scope_skips_unrelated_fallback_results(self):
        engine = router.Router(order=['duckduckgo', 'bing', 'wikipedia'])
        calls = []
        def sources(name, _query):
            calls.append(name)
            if name == 'duckduckgo':
                return []
            if name == 'bing':
                return [{'title': 'Wrong spatial site', 'url': 'https://www.spatial.io/', 'content': ''}]
            if name == 'wikipedia':
                return [{'title': 'Not PostGIS', 'url': 'https://en.wikipedia.org/wiki/GIS', 'content': ''}]
            return [
                {'title': 'Unrelated landing page', 'url': 'https://postgis.net.bad.example/docs', 'content': ''},
                {'title': 'Authoritative GIS docs', 'url': 'https://postgis.net/documentation/', 'content': ''},
                {'title': 'Workshop', 'url': 'https://docs.postgis.net/workshops/index.html', 'content': ''},
            ]
        with patch.object(router, 'provider', side_effect=sources):
            status, payload = engine.search('site:postgis.net spatial index GiST')
        self.assertEqual(status, 200)
        self.assertEqual(calls, ['duckduckgo', 'bing', 'wikipedia', 'searxng'])
        self.assertEqual(payload['provider'], 'searxng')
        self.assertEqual([r['url'] for r in payload['results']], [
            'https://postgis.net/documentation/',
            'https://docs.postgis.net/workshops/index.html',
        ])
        self.assertEqual([r['status'] for r in payload['failed_providers']],
                         ['empty', 'site_mismatch', 'site_mismatch'])

    def test_site_filter_is_case_insensitive_and_rejects_lookalike_domains(self):
        sites = router.site_domains('Read SITE:POSTGIS.net spatial index')
        self.assertEqual(sites, ('postgis.net',))
        for url in ('https://postgis.net/', 'https://docs.postgis.net/index.html'):
            self.assertTrue(router.matches_site(url, sites), url)
        for url in ('https://postgis.net.evil.example/', 'https://fakepostgis.net/',
                    'https://notpostgis.net/', 'https://example.org/'):
            self.assertFalse(router.matches_site(url, sites), url)
        self.assertEqual(router.site_domains('no domain specified'), ())
        self.assertEqual(router.site_domains('site:postgis.net site:docs.example.org'),
                         ('postgis.net', 'docs.example.org'))

    def test_site_scoped_search_returns_error_instead_of_irrelevant_results(self):
        engine = router.Router(order=['bing'])
        with patch.object(router, 'provider', return_value=[
            {'title': 'Irrelevant document', 'url': 'https://www.spatial.io/', 'content': ''},
        ]):
            status, payload = engine.search('site:postgis.net geometry indexing')
        self.assertEqual(status, 503)
        self.assertEqual(payload['results'], [])
        self.assertEqual([x['status'] for x in payload['failed_providers']],
                         ['site_mismatch', 'site_mismatch'])

    def test_cache(self):
        engine = router.Router(order=['bing'], ttl=600)
        with patch.object(router, 'provider', return_value=[{'title': 'Reference', 'url': 'https://docs.example.com', 'content': ''}]) as mock:
            self.assertFalse(engine.search('cache key')[1]['cached'])
            self.assertTrue(engine.search('cache key')[1]['cached'])
        mock.assert_called_once()

    def test_complete_failure_is_http_503(self):
        engine = router.Router(order=['bing'])
        with patch.object(router, 'provider', return_value=[]):
            status, payload = engine.search('nothing exists')
        self.assertEqual(status, 503)
        self.assertEqual(payload['results'], [])

    def test_bing_redirect_is_unwrapped(self):
        encoded = base64.urlsafe_b64encode(b'https://postgis.net/docs/').decode().rstrip('=')
        results = router.normalize([{'title': 'Docs', 'url': 'https://www.bing.com/ck/a?u=a1' + encoded}], 'bing')
        self.assertEqual(results[0]['url'], 'https://postgis.net/docs/')

    def test_html_parsers(self):
        ddg = router.SearchHtml('duckduckgo')
        ddg.feed('<div><a class="result__a" href="https://postgis.net/">PostGIS</a><a class="result__snippet">Spatial database</a></div>')
        self.assertEqual(ddg.items[0]['content'], 'Spatial database')
        bing = router.SearchHtml('bing')
        bing.feed('<li class="b_algo"><h2><a href="https://postgis.net/">PostGIS</a></h2><div><p>Spatial database</p></div></li>')
        self.assertEqual(bing.items[0]['url'], 'https://postgis.net/')

    def test_http_contract(self):
        engine = router.Router(order=['bing'])
        with patch.object(router, 'provider', return_value=[{'title': 'PostGIS docs', 'url': 'https://postgis.net/docs/', 'content': 'Reference'}]):
            httpd = ThreadingHTTPServer(('127.0.0.1', 0), router.handler_factory(engine))
            thread = threading.Thread(target=httpd.serve_forever, daemon=True)
            thread.start()
            try:
                address = 'http://127.0.0.1:' + str(httpd.server_port)
                body = json.load(urlopen(address + '/search?q=PostGIS&format=json&pageno=1'))
                self.assertIn('results', body)
                self.assertEqual(body['results'][0]['title'], 'PostGIS docs')
                self.assertEqual(json.load(urlopen(address + '/health'))['status'], 'ok')
            finally:
                httpd.shutdown()
                httpd.server_close()
                thread.join()

    def test_invalid_search(self):
        self.assertEqual(router.Router().search(' ')[0], 400)
        self.assertEqual(router.normalize([{'title': 'Danger', 'url': 'javascript:alert(1)'}], 'bing'), [])


if __name__ == '__main__':
    unittest.main()
