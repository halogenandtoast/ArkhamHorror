# Arkham Horror LCG: Offline Build System

## Background

[Arkham Horror LCG](https://github.com/halogenandtoast/ArkhamHorror) is a web-based implementation of the Arkham Horror LCG card game.

The upstream project is deployed with Docker. This offline build system provides an **offline build pipeline** that produces a **self-contained distribution package**. In other words, everything required at runtime is bundled during the build, so end users can simply extract the package and run it without installing additional development tools.

The original project source must not be modified permanently. Frontend source patches are allowed only temporarily during the build and must be restored before the build finishes.

## Original Project Architecture (Docker-based)

```text
                    ┌──────────────────────────────────┐
                    │      Nginx (:3000)               │
                    │  ├─ /            → Static files  │
                    │  ├─ /api/*       → Backend :3002 │
                    │  └─ /health      → Backend :3002 │
                    └──────────┬───────────────────────┘
                               │ proxy_pass
                    ┌──────────▼────────────────────┐
                    │   arkham-api (:3002)          │
                    │   (Yesod/Warp Haskell backend)│
                    │   └─ DATABASE_URL → PG        │
                    └──────────┬────────────────────┘
                               │
                    ┌──────────▼──────────────────┐
                    │   PostgreSQL (:5433)        │
                    │   Initialized via setup.sql │
                    │   (full production DB dump) │
                    └─────────────────────────────┘
```

Key files:

- `start.sh` → `PORT=3002 arkham-api & nginx ...`
- `web-entrypoint.sh` → sets `ASSET_HOST` + `DATABASE_URL`
- `prod.nginxconf` → Nginx config (`root /opt/arkham/src/frontend/dist`)
- `setup.sql` → full PostgreSQL dump (includes `uuid-ossp`)
- `Dockerfile` → multi-stage build: frontend (Vite) + backend (Stack/GHC 9.12.2)

## Offline Distribution Architecture

The generated distribution does not depend on Docker and can run directly after extraction.

```text
 ArkhamHorror-{platform}.tar.gz
 └── ArkhamHorror-{platform}/
     ├── bin/
     │   ├── arkham-api          # Haskell backend (port 3002)
     │   └── nginx               # Nginx (port 3000)
     ├── lib/                    # Runtime dynamic libraries (libpq, libpcre, ...)
     ├── frontend/dist/          # Frontend static files (Vite build)
     ├── pgsql/                  # PostgreSQL 14.15 (built from source)
     │   ├── bin/  (postgres, initdb, pg_ctl, pg_isready, psql)
     │   ├── lib/
     │   └── share/
     ├── config/
     │   ├── nginx.conf          # Generated at runtime
     │   ├── mime.types          # Minimal built-in MIME types
     │   └── settings.yml        # Yesod config
     ├── setup.sql               # Full database schema/data import file
     ├── start.sh                # Start script (--status / --stop)
     ├── start.bat               # Windows launcher (double-click to use)
     └── stop.sh                 # Stop shortcut
```

Startup flow:

`start.sh` → PostgreSQL → `arkham-api` (:3002) → Nginx (:3000)

## Tech Stack and Versions

Versions are aligned with `docker-compose.yml` and `Dockerfile`. PostgreSQL uses the latest patch release in the 14.x series to satisfy Haskell build dependencies.

| Component | Technology | Version | Source |
| --- | --- | --- | --- |
| Backend | Haskell (GHC) | 9.12.2 | `downloads.haskell.org` bindist |
| Build Tool | Stack | 3.7.1 | GitHub Releases prebuilt binary |
| Frontend | Node.js + Vite | 22.12.0 LTS | `nodejs.org` prebuilt binary |
| Database | PostgreSQL | 14.15 | `ftp.postgresql.org` source build |
| Proxy | Nginx | 1.26.2 | `nginx.org` source build (minimal) |

GHC / Stack / Node.js use official prebuilt binaries. PostgreSQL and Nginx are built from source to avoid hard-coded paths inside prebuilt packages.

## Directory Layout

```text
offline/
├── build_all.sh
├── README.md
├── build-offline.yml
├── scripts/
│   ├── utils.sh
│   ├── 01-check-project-deps.sh
│   ├── 02-verify-deps.sh
│   ├── 03-build-frontend.sh
│   ├── 04-build-backend.sh
│   └── 05-package.sh
├── _tmp/                        # Download cache (gitignored)
├── _deps/                       # Toolchains and intermediate artifacts (gitignored)
│   ├── ghcup/
│   ├── node/
│   ├── postgres/
│   ├── nginx/
│   ├── node_modules/
│   ├── stack-work/
│   └── arkham-api
└── _dist/                       # Distribution packages (gitignored)
    └── ArkhamHorror-{platform}.tar.gz
```

## Usage

### Local Build

```bash
cd offline
chmod +x build_all.sh scripts/*.sh
./build_all.sh
./build_all.sh --skip-deps
./build_all.sh --clean
./build_all.sh --verbose
```

### Using the Distribution Package

#### Linux / macOS

```bash
tar -xzf ArkhamHorror-macos-arm64.tar.gz
cd ArkhamHorror-macos-arm64
bash start.sh
bash start.sh --status
bash start.sh --stop
# Open http://localhost:3000
```

On first run, `initdb` is executed automatically and `setup.sql` is imported. Later runs reuse existing data. The browser opens automatically after successful startup.

#### Windows

**Just double-click `start.bat`.** It automatically handles WSL detection, installation, user creation, permission setup, and service startup.

High-level first-start flow for Windows users:

1. Double-click `start.bat`.
2. If WSL is missing, install WSL + Ubuntu and reboot if required.
3. Probe available Ubuntu distributions one by one.
4. Ensure the `arkham` user exists.
5. Convert the current Windows path with `wslpath`.
6. Run `bash start.sh` inside WSL as user `arkham`.
7. Place `pgdata` in the OS user data directory under ext4.
8. Start PostgreSQL → `arkham-api` → Nginx.
9. Open the browser automatically at `http://localhost:3000`.
10. Keep the cmd window open to keep the WSL foreground session alive.
11. Closing the window or pressing Ctrl+C automatically stops services, backs up `pgdata`, and clears logs.

```text
Windows first-run flow
══════════════════════

  Double-click start.bat
       │
       ▼
  Is WSL installed?──No──→ Install WSL + Ubuntu automatically
       │                  → Reboot Windows
       │Yes               → Double-click start.bat again
       ▼
  Probe Ubuntu distros one by one
  (wsl -d <name> -- echo ok)
  Ubuntu -> Ubuntu-24.04 -> Ubuntu-22.04 -> ...
       │
       ├── All fail -> Install Ubuntu
       └── Found a working distro ↓
       │
       ▼
  Create arkham user (0002)
       │
       ▼
  Convert path with wslpath (0003)
       │
       ▼
  Run bash start.sh with wsl -u arkham
       │
       ▼
  pgdata lives in the OS user data dir
  (~/.local/share/ArkhamHorror/)
  First start migrates data/pgdata automatically
  or does a fresh initdb on ext4 (no NTFS permission issue)
  Error codes 1xxx~3xxx
       │
       ▼
  Start PostgreSQL (2xxx) -> arkham-api (3xxx) -> Nginx
       │
       ▼
  Startup failed?──Yes──→ Restart Ubuntu and retry once
       │                 → Still fails -> exit (0005)
       ▼
  Open browser automatically
  http://localhost:3000
       │
       ▼
  Keep the cmd window open
  (to keep the WSL session alive)
       │
       ▼
  Close window / Ctrl+C
  -> auto stop + pgdata backup + log cleanup
```


How it works:

1. `start.bat` is the Windows-side entry point. It checks/installs WSL + Ubuntu, creates the `arkham` user, and invokes `start.sh` with `-u arkham`.
2. `start.sh` is the Linux-side entry point. It migrates or initializes `pgdata`, starts all services, and opens the browser automatically.
3. `pgdata` is stored in the OS user data directory:
   - macOS: `~/Library/Application Support/ArkhamHorror/`
   - Linux / WSL: `~/.local/share/ArkhamHorror/`
4. On first start, if `data/pgdata` already exists inside the package, it is migrated automatically to the OS user data directory.
5. On every normal shutdown, `pgdata` is copied back into `data/pgdata` so backups are easier for end users.
6. `pgdata_version` stores a timestamp to help track data version changes.
7. After successful startup, the cmd window intentionally remains open so WSL 2 does not terminate background services due to idle timeout.
8. Closing the cmd window or pressing Ctrl+C stops services automatically through the `trap` logic in `start.sh`.
9. If startup fails, cleanup is automatic: `start.sh` uses an EXIT trap to call `do_stop()`, and `start.bat` also falls back to `start.sh --stop` on the failure path.
10. Everything always runs as user `arkham`, avoiding PostgreSQL refusal to run as root.

Data backup:

Actual game data lives in the OS user data directory:

- macOS: `~/Library/Application Support/ArkhamHorror/pgdata/`
- Linux / WSL: `~/.local/share/ArkhamHorror/pgdata/`

On every normal shutdown, a copy is also written to `data/pgdata/` inside the distribution package. Copying either `data/` or the OS data directory is enough for a full backup.

## Error Code Quick Reference

### `start.bat` (0xxx: WSL environment)

| Code | Meaning |
| --- | --- |
| 0 | Normal (WSL install finished; retry after reboot if required) |
| 0001 | Ubuntu distribution installation failed |
| 0002 | Failed to create the `arkham` user |
| 0003 | WSL path conversion failed (path contains special characters) |
| 0004 | All Ubuntu distribution probes failed |
| 0005 | `start.sh` still failed after restart-and-retry |

### `start.sh` (1xxx: environment preparation)

| Code | Meaning | Related Log |
| --- | --- | --- |
| 1001 | Refused to run as root | — |
| 1002 | `bin/arkham-api` not found | — |
| 1003 | `bin/nginx` not found | — |
| 1004 | `pgsql/bin/` not found | — |
| 1005 | `frontend/dist/` not found | — |
| 1008 | Failed to create `data/` directory | — |
| 1099 | Unknown command-line argument | — |

### `start.sh` (2xxx: PostgreSQL)

| Code | Meaning | Related Log |
| --- | --- | --- |
| 2001 | Failed to create the OS user data directory | — |
| 2002 | Old data migration (`cp`) failed | — |
| 2003 | `initdb` initialization failed | `initdb.log` |
| 2004 | `pg_ctl` start failed | `pg.log` |
| 2005 | `pg_ready` wait timed out | `pg.log` |
| 2006 | `CREATE DATABASE` failed | `psql.log` |
| 2007 | `setup.sql` import failed | `psql.log` |

### `start.sh` (3xxx: application services)

| Code | Meaning | Related Log |
| --- | --- | --- |
| 3002 | nginx failed to start | `error.log` |
| 3003 | API health check timed out | `arkham-api.log` |
| 3004 | PostgreSQL crashed after startup validation | `pg.log` |
| 3005 | API crashed after startup validation | `arkham-api.log` |
| 3006 | nginx crashed after startup validation | `error.log` |

### `start.sh` (4xxx: stop / cleanup, warning level)

| Code | Meaning |
| --- | --- |
| 4004 | Ports still occupied after shutdown (forced cleanup with SIGKILL) |
| 4005 | `pgdata` backup failed |

Log file locations:

All logs live under the distribution package `data/` directory:

- `pg.log` (PostgreSQL)
- `initdb.log` (initialization)
- `psql.log` (SQL import)
- `arkham-api.log` (backend API)
- `error.log` (nginx errors)
- `access.log` (nginx access log)

Logs are cleared automatically after a normal Ctrl+C shutdown. They are preserved when startup fails.

## FAQ

**Q: Where are image assets loaded from?**

A: nginx implements a hybrid strategy: local-first with CDN fallback. Requests under `/img/` are served from disk if the file exists locally; otherwise nginx transparently proxies the request to the CDN. Local images live under `frontend/dist/img/` inside the distribution package.

```text
frontend/dist/img/arkham/
├── cards/            # English cards (for example 01116.avif)
├── zh/cards/         # Chinese translated cards
├── portraits/        # Investigator portraits
├── encounter-sets/   # Encounter cards
├── icons/            # Icons
├── tokens/           # Tokens
└── ...
```

To fetch images, run `./scripts/fetch-assets.sh en` (English, about 1.3 GB) or `./scripts/fetch-assets.sh en+zh` (English + Chinese, about 1.5 GB) in the project root, then copy `frontend/public/img/` into `frontend/dist/img/` in the distribution package. You do not have to download everything; nginx automatically falls back to the CDN for missing files.

**Q: What if `initdb` reports `invalid permissions` on WSL/NTFS?**

A: NTFS does not provide Unix file permissions by default. In the current design, `pgdata` has been moved into WSL ext4 (`~/.local/share/ArkhamHorror/pgdata/`), so the old NTFS metadata fix path is no longer required for normal usage. If the package still lives on an unsupported filesystem such as exFAT or FAT32, move the whole folder to an NTFS partition first.

**Q: Why does `start.bat` request administrator privileges on first launch?**

A: If WSL is not yet installed, `start.bat` runs `wsl --install -d Ubuntu`, which requires elevation. After installation and the required reboot, double-click `start.bat` again and it should start normally.

**Q: Why does `start.bat` say no Ubuntu distribution was found?**

A: WSL may be installed but Ubuntu has not completed installation yet. The script automatically runs `wsl --install -d Ubuntu`; after Ubuntu finishes installing and the initial user setup is complete, double-click `start.bat` again.

**Q: Do I need `sudo` inside WSL?**

A: Normally no. Because `pgdata` is now stored under WSL ext4 instead of NTFS, startup through `start.bat` does not require password prompts in the normal flow.

**Q: Why does the cmd window stay open after double-clicking `start.bat`?**

A: This is expected. The cmd window stays open to keep a foreground WSL session alive; otherwise WSL 2 may reclaim the virtual machine after an idle timeout and kill background services. Closing the window or pressing Ctrl+C stops all services automatically.

