"""A caller's own custom cards, through the API that owns them.

Every request carries the caller's credential and nothing else. This server holds
no signing secret and no user ids on the remote path: it cannot say who anyone is,
only forward what it was given and let arkham-api decide. That is the whole of its
security model, and it is deliberate -- a shared server able to mint its own
tokens could mint one for any account, so it must not be able to.

The cache is per-'Library', never module-level. A cache keyed by "cards" on a
multi-tenant server hands one user's library to the next caller, which is the
single worst bug this file could have.
"""

from __future__ import annotations

import base64
import hashlib
import hmac
import json
import os
import re
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any

from . import paths

API = os.environ.get("ARKHAM_API", "http://localhost:3002")
SETTINGS = paths.settings_yml()
CACHE_TTL_SECONDS = 30
TIMEOUT_SECONDS = 30


class Unauthorized(Exception):
    """No usable credential, or one the API rejected."""


class Forbidden(Exception):
    """A good credential without the scope for this.

    Distinct from Unauthorized on purpose: a client that retries authentication on
    a 401 would loop forever on a permission it is never going to be granted.
    """


class RateLimited(Exception):
    """The API refused for now, not forever."""


class ApiError(Exception):
    pass


# * Credentials


def local_token() -> str:
    """An `Authorization` value for the local single-user case, minted here.

    Only for the stdio server on the developer's own machine, where holding the
    app's signing secret is no worse than holding its database. The remote server
    never calls this -- it has no secret to sign with, which is the point.

    `jwt` must be a number: `tokenToUserId` runs `fromJSON` for a `UserId`, which
    is an Int64 key and will not read `"1"`. A string verifies and still 401s,
    with nothing to say which of the two steps failed.
    """
    user_id = os.environ.get("ARKHAM_USER_ID", "1")
    secret = os.environ.get("JWT_SECRET")
    if not secret and SETTINGS is not None:
        match = re.search(r'jwt-secret:\s*"_env:JWT_SECRET:([^"]+)"', SETTINGS.read_text())
        secret = match.group(1) if match else None
    if not secret:
        raise Unauthorized(f"no JWT secret: set JWT_SECRET, or check {SETTINGS}")

    def segment(payload: dict) -> str:
        raw = json.dumps(payload, separators=(",", ":")).encode()
        return base64.urlsafe_b64encode(raw).rstrip(b"=").decode()

    header = segment({"alg": "HS256", "typ": "JWT"})
    claims = segment(
        {
            "iss": "arkham",
            "iat": int(time.time()),
            "jwt": int(user_id) if user_id.isdigit() else user_id,
        }
    )
    signature = (
        base64.urlsafe_b64encode(
            hmac.new(secret.encode(), f"{header}.{claims}".encode(), hashlib.sha256).digest()
        )
        .rstrip(b"=")
        .decode()
    )
    return f"Token {header}.{claims}.{signature}"


# * The library


class Library:
    """One caller's view of their own cards. Built per request, never per process."""

    def __init__(self, authorization: str | None):
        if not authorization or not authorization.strip():
            raise Unauthorized(
                "This server needs your Arkham Horror API key. Send it as "
                "`Authorization: Bearer ak_...` -- make one under Settings -> API keys,\n"
                "which is admin-only while this settles."
            )
        self.authorization = authorization.strip()
        self._cache: dict[str, tuple[float, Any]] = {}

    @property
    def fingerprint(self) -> str:
        """A stable, non-reversible handle, so a log line or a rate-limit bucket
        can name a caller without ever holding their credential."""
        return hashlib.sha256(self.authorization.encode()).hexdigest()[:16]

    def request(self, method: str, path: str, body: Any = None) -> Any:
        payload = None if body is None else json.dumps(body).encode()
        req = urllib.request.Request(
            f"{API}/api/v1/{path.lstrip('/')}",
            data=payload,
            method=method,
            headers={
                # Forwarded verbatim. Whatever scheme the caller used is the scheme
                # arkham-api sees, so `Token` (a session) and `Bearer` (a key) both
                # work and neither is quietly rewritten into the other.
                "Authorization": self.authorization,
                "Accept": "application/json",
                **({"Content-Type": "application/json"} if payload else {}),
            },
        )
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT_SECONDS) as response:
                raw = response.read()
                return json.loads(raw) if raw else None
        except urllib.error.HTTPError as error:
            detail = error.read().decode(errors="replace")[:800]
            message = _message_of(detail) or error.reason
            if error.code == 401:
                raise Unauthorized(
                    f"{message}. A key goes in `Authorization: Bearer ak_...`; "
                    f"an account session uses `Token <jwt>`."
                ) from error
            if error.code == 403:
                raise Forbidden(str(message)) from error
            if error.code == 429:
                raise RateLimited(str(message)) from error
            raise ApiError(f"{method} {path} -> {error.code}: {message}") from error
        except (urllib.error.URLError, TimeoutError) as error:
            raise ApiError(f"cannot reach the Arkham API at {API}: {error}") from error

    def _cached(self, key: str, fetch) -> Any:
        entry = self._cache.get(key)
        if entry and time.time() - entry[0] < CACHE_TTL_SECONDS:
            return entry[1]
        value = fetch()
        self._cache[key] = (time.time(), value)
        return value

    def invalidate(self) -> None:
        """Called after a write, so the next read does not answer from before it."""
        self._cache.clear()

    # * Who am I

    def whoami(self) -> dict:
        """The caller and their scopes, as the API reports them.

        Asked of the API rather than worked out here: this server must not decide
        what a credential may do, only relay the answer. Used to offer a read-only
        key the read tools alone, instead of writes that would 403.
        """
        return self._cached("self", lambda: self.request("GET", "api-keys/self") or {})

    def scopes(self) -> list[str]:
        try:
            found = self.whoami().get("scopes")
            return [s for s in found if isinstance(s, str)] if isinstance(found, list) else []
        except (Unauthorized, Forbidden):
            raise
        except ApiError:
            # An API without /api-keys/self should not make the server useless;
            # let each call be its own authority instead.
            return []

    # * Sets

    def sets(self) -> list[dict]:
        rows = self._cached("sets", lambda: self.request("GET", "arkham/custom-card-sets") or [])
        out = []
        for row in rows:
            entity = row.get("json", row)
            out.append(
                {
                    "id": row.get("id") or entity.get("id"),
                    "name": entity.get("name"),
                    "sourceCode": entity.get("sourceCode"),
                }
            )
        return out

    def create_set(self, name: str) -> dict:
        row = self.request("POST", "arkham/custom-card-sets", {"name": name})
        self.invalidate()
        entity = (row or {}).get("json", row) or {}
        return {
            "id": (row or {}).get("id") or entity.get("id"),
            "name": entity.get("name") or name,
        }

    def rename_set(self, set_id: str, name: str) -> dict:
        row = self.request("PUT", f"arkham/custom-card-sets/{set_id}", {"name": name})
        self.invalidate()
        entity = (row or {}).get("json", row) or {}
        return {"id": set_id, "name": entity.get("name") or name}

    def delete_set(self, set_id: str) -> dict:
        self.request("DELETE", f"arkham/custom-card-sets/{set_id}")
        self.invalidate()
        return {"deleted": set_id, "note": "every card in it went with it"}

    # * Cards

    def cards(self) -> list[dict]:
        rows = self._cached("cards", lambda: self.request("GET", "arkham/custom-cards") or [])
        out = []
        for row in rows:
            entity = row.get("json", row)
            out.append(
                {
                    "id": row.get("id"),
                    "cardCode": entity.get("cardCode"),
                    "setId": entity.get("customCardSetId"),
                    "def": entity.get("def") or {},
                    "art": entity.get("art"),
                }
            )
        return out

    def find(self, code_or_name: str) -> dict | None:
        wanted = code_or_name.strip().lower()
        cards = self.cards()
        for card in cards:
            if (card.get("cardCode") or "").lower() == wanted:
                return card
        for card in cards:
            if title_of(card["def"]).lower() == wanted:
                return card
        for card in cards:
            if wanted and wanted in title_of(card["def"]).lower():
                return card
        return None

    def save(self, card_def: dict, set_id: str, art: str | None = None) -> dict:
        row = self.request(
            "POST", "arkham/custom-cards", {"setId": set_id, "def": card_def, "art": art}
        )
        self.invalidate()
        entity = (row or {}).get("json", row) or {}
        return {
            "saved": True,
            "id": (row or {}).get("id"),
            "cardCode": entity.get("cardCode") or card_def.get("cardCode"),
            "note": "a 200 here is the engine's own parse check on the def",
        }

    def delete_card(self, code_or_name: str) -> dict:
        card = self.find(code_or_name)
        if not card:
            raise ApiError(f"no card matching {code_or_name!r} in your library")
        self.request("DELETE", f"arkham/custom-cards/{card['id']}")
        self.invalidate()
        return {"deleted": card["cardCode"], "name": title_of(card["def"])}

    # * Precedent

    def examples(self, construct: str, limit: int = 8) -> list[dict]:
        """Cards whose behaviour uses a named construct, and the fragment that does.

        A fragment a player has actually used is known to decode, which is more
        than the schema can promise.
        """
        needle = construct.strip()
        found: list[dict] = []
        for card in self.cards():
            behaviour = {
                key: value
                for key, value in (card["def"].get("meta") or {}).items()
                if key.startswith("_")
            }
            for path, value in _walk(behaviour):
                hit = (
                    (isinstance(value, dict) and needle in value)
                    or (isinstance(value, str) and value == needle)
                    or (isinstance(value, dict) and value.get("tag") == needle)
                )
                if not hit:
                    continue
                found.append(
                    {
                        "card": title_of(card["def"]),
                        "cardCode": card["cardCode"],
                        "cardType": card["def"].get("cardType"),
                        "at": f"meta.{path}",
                        "fragment": value
                        if isinstance(value, dict)
                        else {path.split(".")[-1]: value},
                    }
                )
                break
            if len(found) >= limit:
                break
        return found


def _message_of(body: str) -> str | None:
    try:
        parsed = json.loads(body)
    except json.JSONDecodeError:
        return body.strip() or None
    if isinstance(parsed, dict):
        for key in ("message", "error"):
            if isinstance(parsed.get(key), str):
                return parsed[key]
        if isinstance(parsed.get("messages"), list):
            return "; ".join(str(m) for m in parsed["messages"])
    return body.strip() or None


def _walk(value: Any, path: str = ""):
    yield path, value
    if isinstance(value, dict):
        for key, item in value.items():
            yield from _walk(item, f"{path}.{key}" if path else key)
    elif isinstance(value, list):
        for index, item in enumerate(value):
            yield from _walk(item, f"{path}[{index}]")


def title_of(card_def: dict) -> str:
    name = card_def.get("name")
    if isinstance(name, dict):
        subtitle = name.get("subtitle")
        return f"{name.get('title')}" + (f": {subtitle}" if subtitle else "")
    return str(name)


def summarise(card: dict) -> dict:
    card_def = card["def"]
    meta = card_def.get("meta") or {}
    return {
        "cardCode": card["cardCode"],
        "name": title_of(card_def),
        "cardType": card_def.get("cardType"),
        "set": meta.get("set") if isinstance(meta.get("set"), str) else None,
        "abilities": len(meta.get("_abilities") or []),
        "handlers": len(meta.get("_handlers") or []),
        "modifiers": len(meta.get("_modifiers") or []),
        "traits": sorted(card_def.get("cardTraits") or []),
    }
