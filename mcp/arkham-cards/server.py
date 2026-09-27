#!/usr/bin/env python3
"""The local MCP server: stdio, one user, this machine.

The tools themselves live in `lib/tools.py` and are shared with `http_server.py`,
so a tool cannot exist locally and be missing remotely, or be scoped differently
in the two. All this file does is speak the protocol over stdin and stdout, and
supply the one credential a single-user server needs.

That credential is minted here from the app's own signing secret, which is
defensible on the machine that holds the database anyway and is exactly what the
remote server must never do -- see the note at the top of `http_server.py`.

For the remote, multi-user server, run `http_server.py` instead.
"""

from __future__ import annotations

import json
import sys
import traceback
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from lib import library, tools  # noqa: E402

PROTOCOL_VERSION = "2025-06-18"
SERVER_INFO = {"name": "arkham-cards", "version": "1.0.0"}

# Minted on demand, so the reference tools still answer on a machine with no API
# running and no secret to hand.
CONTEXT = tools.Context(token_factory=library.local_token)


def handle(message: dict) -> dict | None:
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

    if method == "tools/list":
        # Every tool: the local caller owns the account, so there is nothing to gate.
        return {"jsonrpc": "2.0", "id": request_id, "result": {"tools": tools.TOOLS}}

    if method == "prompts/list":
        return {"jsonrpc": "2.0", "id": request_id, "result": {"prompts": tools.PROMPTS}}

    if method == "prompts/get":
        try:
            messages = tools.prompt_messages(params.get("name", ""), params.get("arguments") or {})
        except KeyError:
            return {
                "jsonrpc": "2.0",
                "id": request_id,
                "error": {"code": -32602, "message": f"no prompt named {params.get('name')!r}"},
            }
        return {"jsonrpc": "2.0", "id": request_id, "result": {"messages": messages}}

    if method == "tools/call":
        name = params.get("name")
        arguments = params.get("arguments") or {}
        try:
            payload = {"content": [{"type": "text", "text": tools.call(name, arguments, CONTEXT)}]}
        except KeyError:
            return {
                "jsonrpc": "2.0",
                "id": request_id,
                "error": {"code": -32602, "message": f"no tool named {name!r}"},
            }
        except TypeError as bad_arguments:
            payload = {
                "content": [{"type": "text", "text": f"{name}: {bad_arguments}"}],
                "isError": True,
            }
        except Exception as failure:  # noqa: BLE001 - the client is the only place to report
            payload = {
                "content": [
                    {
                        "type": "text",
                        "text": f"{name} failed: {failure}\n\n{traceback.format_exc(limit=4)}",
                    }
                ],
                "isError": True,
            }
        return {"jsonrpc": "2.0", "id": request_id, "result": payload}

    if request_id is None:
        return None
    return {
        "jsonrpc": "2.0",
        "id": request_id,
        "error": {"code": -32601, "message": f"unknown method {method!r}"},
    }


def main() -> int:
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            message = json.loads(line)
        except json.JSONDecodeError:
            continue
        try:
            response = handle(message)
        except Exception as failure:  # noqa: BLE001
            response = {
                "jsonrpc": "2.0",
                "id": message.get("id"),
                "error": {"code": -32603, "message": str(failure)},
            }
        if response is not None:
            sys.stdout.write(json.dumps(response) + "\n")
            sys.stdout.flush()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
