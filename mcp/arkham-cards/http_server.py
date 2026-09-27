#!/usr/bin/env python3
"""The remote MCP server: one HTTP endpoint, one caller's credential per request.

Speaks MCP's Streamable HTTP transport at `POST /mcp`, statelessly. There is no
session store and no `Mcp-Session-Id`, because there is no per-connection state to
keep: the credential arrives on every request and nothing is remembered between
them. That makes the process restartable and replicable for free, and it means a
leaked session id is not a thing that exists.

**This server holds no secret and decides no permissions.** It forwards the
caller's `Authorization` header to arkham-api and relays the answer. It cannot
mint a token, cannot name a user, and cannot widen a scope -- so a bug here cannot
become access to somebody's account. The scope hints in `lib/tools.py` only decide
which tools a caller is *offered*; every call is checked again by arkham-api,
which is the only thing that should be checking.

Run it behind a reverse proxy that terminates TLS. Bearer credentials over plain
HTTP are credentials in the clear, and this process does not do TLS.
"""

from __future__ import annotations

import json
import os
import sys
import threading
import time
import traceback
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from lib import library, tools  # noqa: E402

PROTOCOL_VERSION = "2025-06-18"
SERVER_INFO = {"name": "arkham-cards", "version": "1.0.0"}

HOST = os.environ.get("MCP_HOST", "127.0.0.1")
PORT = int(os.environ.get("MCP_PORT", "8420"))
PATH = os.environ.get("MCP_PATH", "/mcp")
MAX_BODY_BYTES = int(os.environ.get("MCP_MAX_BODY", str(4 * 1024 * 1024)))

# Where the server is reachable, for the OAuth-style discovery document and for
# the `resource_metadata` hint on a 401. Only used to describe itself.
PUBLIC_URL = os.environ.get("MCP_PUBLIC_URL", f"http://{HOST}:{PORT}{PATH}")
AUTH_DOCS_URL = os.environ.get("MCP_AUTH_DOCS_URL", "")

# Browsers are not the intended client, but a page in one could POST here with a
# user's ambient credentials if any existed; the spec asks for an Origin check to
# shut the DNS-rebinding case. An allowlist of "" means "refuse any Origin at all",
# which is right for a server whose clients never send one.
ALLOWED_ORIGINS = [o for o in os.environ.get("MCP_ALLOWED_ORIGINS", "").split(",") if o]

# A coarse per-credential request ceiling, to keep one runaway client from
# flooding arkham-api with reads. Writes are capped by the API itself, on the key
# row, which is the limit that actually matters -- this one only protects the hop.
REQUESTS_PER_MINUTE = int(os.environ.get("MCP_REQUESTS_PER_MINUTE", "240"))
_buckets: dict[str, tuple[float, int]] = {}
_buckets_lock = threading.Lock()


def over_rate_limit(fingerprint: str) -> bool:
    now = time.time()
    with _buckets_lock:
        started, count = _buckets.get(fingerprint, (now, 0))
        if now - started >= 60:
            started, count = now, 0
        count += 1
        _buckets[fingerprint] = (started, count)
        if len(_buckets) > 10000:
            # Unbounded growth is the only leak a stateless server can have.
            for key in [k for k, (t, _) in _buckets.items() if now - t > 300]:
                _buckets.pop(key, None)
        return count > REQUESTS_PER_MINUTE


def error(request_id, code: int, message: str, data=None) -> dict:
    payload = {"jsonrpc": "2.0", "id": request_id, "error": {"code": code, "message": message}}
    if data is not None:
        payload["error"]["data"] = data
    return payload


def handle(message: dict, authorization: str | None) -> dict | None:
    """One JSON-RPC message. Returns the response, or None for a notification."""
    method = message.get("method")
    request_id = message.get("id")
    params = message.get("params") or {}

    if method == "initialize":
        return {
            "jsonrpc": "2.0",
            "id": request_id,
            "result": {
                "protocolVersion": params.get("protocolVersion") or PROTOCOL_VERSION,
                "capabilities": {"tools": {}, "prompts": {}},
                "serverInfo": SERVER_INFO,
            },
        }

    if method in ("notifications/initialized", "initialized"):
        return None

    if method == "ping":
        return {"jsonrpc": "2.0", "id": request_id, "result": {}}

    if method == "prompts/list":
        return {"jsonrpc": "2.0", "id": request_id, "result": {"prompts": tools.PROMPTS}}

    if method == "prompts/get":
        try:
            messages = tools.prompt_messages(params.get("name", ""), params.get("arguments") or {})
        except KeyError:
            return error(request_id, -32602, f"no prompt named {params.get('name')!r}")
        return {"jsonrpc": "2.0", "id": request_id, "result": {"messages": messages}}

    context = tools.Context(authorization)

    if method == "tools/list":
        # Offer only what this credential can use, so a read-only key is not shown
        # writes that would 403. The API is still the one that decides.
        try:
            scopes = context.scopes()
        except library.Unauthorized as unauthorized:
            return error(request_id, -32001, str(unauthorized))
        except library.ApiError as failure:
            return error(request_id, -32002, str(failure))
        return {"jsonrpc": "2.0", "id": request_id, "result": {"tools": tools.listing(scopes)}}

    if method == "tools/call":
        name = params.get("name")
        arguments = params.get("arguments") or {}
        try:
            return {
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {"content": [{"type": "text", "text": tools.call(name, arguments, context)}]},
            }
        except KeyError:
            return error(request_id, -32602, f"no tool named {name!r}")
        except TypeError as bad_arguments:
            return {
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "content": [{"type": "text", "text": f"{name}: {bad_arguments}"}],
                    "isError": True,
                },
            }
        except (
            library.Unauthorized,
            library.Forbidden,
            library.RateLimited,
            library.ApiError,
        ) as refused:
            # A tool-level error, not a protocol one: the model should read it and
            # act, which it cannot do with a transport error it never sees.
            return {
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "content": [{"type": "text", "text": f"{type(refused).__name__}: {refused}"}],
                    "isError": True,
                },
            }
        except Exception as failure:  # noqa: BLE001
            print(f"{name} failed: {failure}\n{traceback.format_exc()}", file=sys.stderr, flush=True)
            return {
                "jsonrpc": "2.0",
                "id": request_id,
                "result": {
                    "content": [{"type": "text", "text": f"{name} failed: {failure}"}],
                    "isError": True,
                },
            }

    if request_id is None:
        return None
    return error(request_id, -32601, f"unknown method {method!r}")


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"
    server_version = "arkham-cards-mcp/1.0"

    def log_message(self, fmt: str, *args) -> None:
        # Never the Authorization header, and never a request body: a card def is
        # somebody's unpublished work.
        print(f"{self.address_string()} {fmt % args}", file=sys.stderr, flush=True)

    # * Plumbing

    def _send(self, status: int, payload: dict | None, extra: dict | None = None) -> None:
        body = b"" if payload is None else json.dumps(payload).encode()
        self.send_response(status)
        if body:
            self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        for key, value in (extra or {}).items():
            self.send_header(key, value)
        self.end_headers()
        if body:
            self.wfile.write(body)

    def _unauthorized(self, detail: str) -> None:
        """401 with the challenge a client needs to know what to send.

        `resource_metadata` points at this server's own description, which is what
        an OAuth-capable client follows to discover how to authorize. Until an
        authorization server exists, the document says "use an API key", which is
        a truthful answer to the same question.
        """
        challenge = f'Bearer realm="arkham-cards", resource_metadata="{PUBLIC_URL}/.well-known/oauth-protected-resource"'
        self._send(
            401,
            {"error": "unauthorized", "message": detail},
            {"WWW-Authenticate": challenge},
        )

    def _origin_ok(self) -> bool:
        origin = self.headers.get("Origin")
        return origin is None or origin in ALLOWED_ORIGINS

    # * Routes

    def do_GET(self) -> None:  # noqa: N802
        # Matched by suffix, not equality: nginx proxies /mcp here without
        # stripping the prefix, so what arrives is `/mcp/health`. The discovery
        # document below was already written this way; this was not, and 404'd in
        # production while working locally.
        if self.path.rstrip("/").endswith(("/health", "/healthz")) or self.path.rstrip("/") in (
            "/health",
            "/healthz",
        ):
            return self._send(200, {"ok": True, "server": SERVER_INFO})
        if self.path.endswith("/.well-known/oauth-protected-resource"):
            # Described the OAuth way so a client that looks finds something, while
            # saying plainly that the credential today is an API key.
            return self._send(
                200,
                {
                    "resource": PUBLIC_URL,
                    "resource_name": "Arkham Horror custom cards",
                    "bearer_methods_supported": ["header"],
                    "scopes_supported": [tools.CARDS_READ, tools.CARDS_WRITE],
                    "authorization_servers": [],
                    "resource_documentation": AUTH_DOCS_URL or None,
                    "note": (
                        "Authenticate with a personal API key: Settings -> API keys in the "
                        "Arkham Horror web app (admin accounts only while this settles), then "
                        "send `Authorization: Bearer ak_...`. "
                        "No authorization server is offered yet, so there is no OAuth flow "
                        "to follow."
                    ),
                },
            )
        if self.path.rstrip("/") == PATH.rstrip("/"):
            # No SSE stream: nothing here initiates a message to the client, so
            # there is nothing for a stream to carry.
            return self._send(405, {"error": "method_not_allowed", "message": "POST to " + PATH})
        self._send(404, {"error": "not_found"})

    def do_DELETE(self) -> None:  # noqa: N802
        # Stateless: there is no session to end.
        self._send(405, {"error": "method_not_allowed", "message": "this server keeps no sessions"})

    def do_POST(self) -> None:  # noqa: N802
        if self.path.rstrip("/") != PATH.rstrip("/"):
            return self._send(404, {"error": "not_found"})
        if not self._origin_ok():
            return self._send(403, {"error": "forbidden", "message": "Origin not allowed"})

        length = int(self.headers.get("Content-Length") or 0)
        if length > MAX_BODY_BYTES:
            return self._send(413, {"error": "too_large", "message": f"max {MAX_BODY_BYTES} bytes"})
        try:
            raw = self.rfile.read(length) if length else b""
            message = json.loads(raw or b"{}")
        except (json.JSONDecodeError, OSError):
            return self._send(400, error(None, -32700, "parse error"))

        authorization = self.headers.get("Authorization")
        if not authorization:
            return self._unauthorized(
                "Send your Arkham Horror API key as `Authorization: Bearer ak_...`. "
                "Make one under Settings -> API keys, which is admin-only while this settles."
            )

        fingerprint = library.Library(authorization).fingerprint
        if over_rate_limit(fingerprint):
            return self._send(
                429,
                error(
                    message.get("id") if isinstance(message, dict) else None,
                    -32003,
                    f"more than {REQUESTS_PER_MINUTE} requests a minute from this credential",
                ),
                {"Retry-After": "60"},
            )

        # A batch is a list; the spec allows one and clients do send them.
        if isinstance(message, list):
            responses = [r for r in (handle(m, authorization) for m in message) if r is not None]
            return self._send(200, responses) if responses else self._send(202, None)

        if not isinstance(message, dict):
            return self._send(400, error(None, -32600, "invalid request"))

        response = handle(message, authorization)
        # A notification gets no body; the spec asks for 202 rather than an empty 200.
        self._send(202, None) if response is None else self._send(200, response)


def main() -> int:
    server = ThreadingHTTPServer((HOST, PORT), Handler)
    server.daemon_threads = True
    # Said out loud at boot, because "this process cannot impersonate anyone" is
    # the load-bearing claim of the whole design and it is invisible otherwise.
    secret_warning = (
        "  WARNING: JWT_SECRET is visible to this process. It is never read here, but a\n"
        "           process holding it can mint a token for any user -- unset it.\n"
        if os.environ.get("JWT_SECRET")
        else "  no signing secret in this environment, so it cannot mint a token for anyone\n"
    )
    print(
        f"arkham-cards MCP on http://{HOST}:{PORT}{PATH}  (public: {PUBLIC_URL})\n"
        f"  arkham-api: {library.API}\n"
        f"  auth: per-request Authorization header, forwarded verbatim\n"
        f"{secret_warning}"
        f"  rate limit: {REQUESTS_PER_MINUTE} requests/min per credential",
        file=sys.stderr,
        flush=True,
    )
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
