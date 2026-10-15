#!/usr/bin/env python3
"""Test-only OpenAI-compatible fake model; forces tool use without GPU or tokens."""
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import re
import sys


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        return

    def do_GET(self):
        body = json.dumps({'object': 'list', 'data': [{'id': 'qwen3.8-27b', 'object': 'model'}]}).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        raw = self.rfile.read(int(self.headers.get('Content-Length', '0')))
        req = json.loads(raw)
        messages = req.get('messages', [])
        names = [x.get('function', {}).get('name') for x in req.get('tools', [])]
        used = any(x.get('role') == 'tool' for x in messages)
        print('mock request', json.dumps({'path': self.path, 'stream': req.get('stream'), 'tools': names, 'used': used, 'roles': [x.get('role') for x in messages][-5:]}), file=sys.stderr, flush=True)
        if not used and any(x in names for x in ('web_search_tool', 'web_search')):
            name = 'web_search_tool' if 'web_search_tool' in names else 'web_search'
            call = {'id':'call_e2e1','type':'function','function':{'name':name,'arguments':'{"query":"PostGIS spatial index official documentation"}'}}
            msg = {'role': 'assistant', 'content': None, 'tool_calls': [call]}
            reason = 'tool_calls'
        else:
            content = 'Tool not registered'
            if used:
                tool_content = str(next((x.get('content', '') for x in reversed(messages) if x.get('role') == 'tool'), ''))
                print('mock tool return:', repr(tool_content[:500]), file=sys.stderr, flush=True)
                match = re.search(r'https?://[^\s\"\x27<>]+', tool_content)
                content = 'Search tool executed; first source URL: ' + (match.group(0).rstrip('\\,})]') if match else '(tool gave no URLs)')
            msg = {'role':'assistant', 'content':content}
            reason = 'stop'
        if req.get('stream'):
            delta = {k:v for k,v in msg.items() if k != 'role'}
            delta['role'] = 'assistant'
            if 'tool_calls' in delta:
                delta['tool_calls'] = [{**delta['tool_calls'][0], 'index':0}]
            parts = [
                {'id':'chatcmpl-mock','object':'chat.completion.chunk','model':req.get('model','qwen3.8-27b'),'choices':[{'index':0,'delta':delta,'finish_reason':None}]},
                {'id':'chatcmpl-mock','object':'chat.completion.chunk','model':req.get('model','qwen3.8-27b'),'choices':[{'index':0,'delta':{},'finish_reason':reason}]},
            ]
            body = ''.join('data: '+json.dumps(p)+'\n\n' for p in parts) + 'data: [DONE]\n\n'
            data = body.encode()
            self.send_response(200)
            self.send_header('Content-Type', 'text/event-stream')
            self.send_header('Content-Length', str(len(data)))
            self.end_headers()
            self.wfile.write(data)
        else:
            data = json.dumps({'id':'chatcmpl-mock','object':'chat.completion','created':1792000000,'model':req.get('model','qwen3.8-27b'),'choices':[{'index':0,'message':msg,'finish_reason':reason}], 'usage':{'prompt_tokens':50,'completion_tokens':40,'total_tokens':90}}).encode()
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(len(data)))
            self.end_headers()
            self.wfile.write(data)


if __name__ == '__main__':
    print('mock model listening on 127.0.0.1:8990', file=sys.stderr, flush=True)
    ThreadingHTTPServer(('127.0.0.1', 8990), Handler).serve_forever()
