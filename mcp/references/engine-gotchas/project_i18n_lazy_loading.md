---
title: i18n locale files should be lazy-loadable
description: Frontend i18n migration plan — keys must be organizable into separately-loadable files so vue-i18n can fetch only what's needed
---
The Arkham frontend i18n (vue-i18n) currently loads the entire `locales/en/` tree eagerly. The user wants to migrate to lazy loading where only the keys a given screen needs are fetched.

**Why:** The locale tree is large and growing (full campaign + scenario + per-card text). Bundling everything blows up the initial JS payload.

**How to apply:** When picking i18n keys/scopes for backend work, pick scopes whose keys naturally cluster into one file at the leaf level (e.g. one scenario, one encounter set, one card). Avoid scopes that scatter related keys across many files or force a global lookup. The structural rule: a scope at depth N should be expressible as a single JSON file at `locales/en/<path>.json`.
