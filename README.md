# Arkham Horror LCG

![Screenshot](./docs/img/screenshot.png)

The goal of this project is to implement a web version of Arkham Horror with as
many of the rules implemented as possible.

## Features

* Multiplayer up to 4 players
* Multiplayer solitaire
* Tarot Readings
* Deck import from ArkhamDB and arkham.build

## Finished Content

### Chapter 1

All content is implemented. You can use every player card (including promos and
parallels) and play every campaign and scenario (including challenge
scenarios). The only thing currently missing are some of the refractions,
designer dares, and other variants.

### Chapter 2

All content before 2027 is implemented, player cards included.

* Brethren of Ash
* Children of Blood (Beta)
* Traces to Nowhere (Beta, side story)

Beta content is hidden until you ask for it: switch on **Enroll in beta** in
Settings and it turns up in the chooser with a beta badge. Expect it to break,
sometimes badly enough to lose the game, and file a bug when it does — that is
what it is in beta for.

### Homebrew

There is fan-made content in here too: four campaigns and nine side stories
written by people in the community. Turn on beta and they show up in the
chooser alongside everything else.

Want to make your own? See [Writing homebrew content](./docs/homebrew.md).

## I just want to try this out on my computer

### Prebuilt package (no Docker)

The least work, if there is a build for your machine. One archive with the
backend, the frontend, PostgreSQL and nginx already in it — nothing to
install, not even Docker.

Grab the archive for your platform from the
[releases page](https://github.com/halogenandtoast/ArkhamHorror/releases):

| You have | Download | Then |
| --- | --- | --- |
| An Apple Silicon Mac | `ArkhamHorror-macos-arm64.tar.gz` | double-click `Start-ArkhamHorror.command` |
| Windows | `ArkhamHorror-linux-x86_64.tar.gz` | double-click `Start-ArkhamHorror.bat` |
| Linux on x86_64 | `ArkhamHorror-linux-x86_64.tar.gz` | `bash game/start.sh` |

Yes, Windows takes the Linux archive: the launcher runs it inside WSL. It
installs WSL and Ubuntu if they are missing, which wants a reboot the first
time, and then you double-click again. Leave the terminal window it opens alone
— closing it or pressing Ctrl+C is how you stop the game, and it writes a
database backup on the way out.

The first start initialises its own database, imports `setup.sql`, brings up all
three services and opens http://localhost:3000. Later starts reuse the data.
`bash game/start.sh --status` and `--stop` do what they say.

Your saved games are kept outside the package, so a newer one does not disturb
them:

* macOS — `~/Library/Application Support/ArkhamHorror/`
* Linux and WSL — `~/.local/share/ArkhamHorror/`

A normal shutdown also writes a dump to the package's own `backup/`, which is
what a later install restores from if it finds no live database.

Images come from the CDN, and nginx falls back to it file by file, so you can
drop as much or as little local art as you like into `game/frontend/dist/img/`.
Your own card images go in `cards/` (or `cards_en/`), which are checked first.

To upgrade, download the new archive into the same folder as the old one and
double-click `Update-ArkhamHorror.command` (`.bat` on Windows). It stops the
services, swaps `game/` for the new one and leaves `cards/`, `cards_en/` and
`backup/` where they are. It will not download anything itself.

`offline/README.md` goes deeper: the codes `start.sh` exits with, which log each
failure lands in, and the order card images are looked up in.

### Linux Users

Install [Docker][docker], then run:

```
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/install.sh | bash
```

This creates an `arkham-horror/` directory, downloads the required files, generates
a database password, and starts the app. Open http://localhost:3000 when it's done.

The script will ask if you want to download game images (~2.9 GB). If you skip
this step, the app loads images from the CDN automatically — no extra setup needed.

### Windows Users

- Install Docker, specifically the version of Docker known as [Docker Desktop](https://docs.docker.com/desktop/).
- Install a "terminal only" version of Ubuntu that can be run inside of Windows, for which the easiest available method is to [install from the Microsoft Store](https://documentation.ubuntu.com/wsl/latest/howto/install-ubuntu-wsl2/#method-3-install-from-the-microsoft-store). You may choose any Linux distro other than Ubuntu if you know what you are doing.
- In Docker Desktop, go to `Settings > Resources > WSL Integration` and enable integration with Ubuntu Linux distro that you installed in the previous step. This allows us to access Docker from inside Ubuntu.
- Start the Ubuntu terminal and run:
  ```
  curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/install.sh | bash
  ```
- The app should start being served at http://localhost:3000, if nothing went wrong.
- Every time you want to start the app, you can use `docker compose up`
- If you get around to figuring out how to use Docker Desktop, you could also press buttons to start and stop the app instead of typing commands to run it.

#### Things that can go wrong

If the app doesnt start at all, or it starts but you cannot create a user account, or any other issue due to Murphy's Law, you have to check the logs of the Docker container to find the error messages. In Docker Desktop, you can access this by clicking on the `arkham-horror` "compose stack" found in the `Containers` list.

Possible problems:

| Problem | Solution |
| ------------- | ------------- |
| Error involving `docker-credential-desktop.exe` | Try restarting your computer |
| When clicking on the "REGISTER" button, the error `password authentication failed` is logged| Stop the app using `docker compose down` and then refetch the app using `docker compose pull`

### Manual setup (alternative)

If you prefer not to use the install script, you'll need four files:

```
mkdir -p arkham-horror/config arkham-horror/scripts arkham-horror/frontend/public/img
cd arkham-horror
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/docker-compose.yml -o docker-compose.yml
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/setup.sql -o setup.sql
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/scripts/fetch-assets.sh -o scripts/fetch-assets.sh
# Generate a strong password
openssl rand -base64 32 > config/postgres_password.txt
docker compose up -d
```

The app automatically detects whether local images are present:
- **No local images** (default after install) → images load from the CDN automatically
- **Local images present** (after running the fetch service) → images served locally

To fetch images locally, use the `fetch-images` Docker service.
Once fetched, restart with `docker compose restart web` and the app will switch to local images.

```
# English only (~1.3 GB) — portraits, tokens, icons, card images
docker compose --profile fetch-images run --rm fetch-images en

# English + a specific language (recommended for non-English play)
docker compose --profile fetch-images run --rm fetch-images en+fr   # French
docker compose --profile fetch-images run --rm fetch-images en+es   # Spanish
docker compose --profile fetch-images run --rm fetch-images en+ita  # Italian
docker compose --profile fetch-images run --rm fetch-images en+ko   # Korean
docker compose --profile fetch-images run --rm fetch-images en+zh   # Chinese

# A single language's card translations only (English assets still load from CDN)
docker compose --profile fetch-images run --rm fetch-images fr

# Everything (~2.9 GB)
docker compose --profile fetch-images run --rm fetch-images all
```

Images are stored in `frontend/public/img/` and mounted into the container.
After fetching, run `docker compose restart web` to pick them up.

To switch back to CDN at any time, add this to the `web` service environment in `docker-compose.yml`:

```yaml
    environment:
      - ASSET_HOST=https://assets.arkhamhorror.app
```

### Updating

Use the upgrade script. It auto-detects your install type (Docker or git
checkout), refreshes control files, pulls the latest images, applies database
migrations, recreates the containers, and re-syncs local images if you have
them.

Run it from anywhere:

```
curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/upgrade.sh | bash
```

Or, from inside your install directory (or a git checkout):

```
./upgrade.sh
```

Options (environment variables):

| Variable | Purpose |
| --- | --- |
| `ARKHAM_INSTALL_DIR` | Install directory to upgrade (default: `arkham-horror`). Set this if you installed somewhere else. |
| `ARKHAM_IMAGE_TARGET` | Force an image-sync target (e.g. `en`, `en+fr`, `all`) instead of auto-detecting from what's already on disk. |

```
# Upgrade an install in a custom directory
ARKHAM_INSTALL_DIR=~/games/arkham curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/upgrade.sh | bash

# Force re-syncing English + French images
ARKHAM_IMAGE_TARGET=en+fr ./upgrade.sh
```

## Local dev

This is the non-Docker path: the servers run on your machine, from the source
tree, with rebuilds and hot reload. The Docker instructions above are a
different thing and need none of this.

### Setup

```
./scripts/setup
```

It checks for everything the build needs, prints whatever is missing with the
command that fixes it, and installs them if you say yes. Then it installs the node
modules, creates the database, applies the migrations, and builds the backend
once.

`--check` reports and changes nothing. `--yes` skips the prompt, `--no-build`
leaves out the backend build, `--no-db` leaves the database alone, and
`--with-images` also fetches the image assets.

What it looks for, if you would rather do it yourself:

* ghcup, and GHC 9.14.1 through it. `backend/stack.yaml` sets `install-ghc:
  false`, so stack will not fetch a compiler of its own — it wants
  `ghc-9.14.1` on PATH, whatever plain `ghc` happens to be.
* Stack 3.7.1 or newer
* Node 20.19 or newer, which is vite 7's floor
* Postgres, with something answering on localhost:5432
* `libpq` and `gmp`, which `postgresql-libpq` and GHC link against

The versions are read out of `backend/stack.yaml` rather than written into the
script, so a resolver bump carries them along.

### Running the server

```
./scripts/boot
```

All three servers in one terminal: the API on :3002, the frontend on :8080, and
the third edition frontend on :8081. A status block sits at the top of the
terminal saying what each one is up to — building and which module, build
failed, hot reload — while their output scrolls past underneath. The keys stop
and start them one at a time:

| Key | |
| --- | --- |
| `a` `f` `t` | stop or start the api, the frontend, the 3ed frontend |
| `A` `F` `T` | restart it |
| `l` | cycle which server's output is shown |
| `s` | print a status block |
| `b` | open the frontend in a browser |
| `q` | quit, stopping everything |

The backend rebuilds and restarts itself as you edit, vite keeps its hot
reload, and each server's output also lands in `tmp/dev/`.

`--no-3ed` leaves the third edition frontend out and `--only api,web` picks
what to run. `DEBUG=1 ./scripts/boot` turns on the API's info logging (see
`Arkham.Debug`). `API_TARGET=api.watch.test.entr.log ./scripts/boot` runs the
test suite on every rebuild too. It always runs the frontends with an empty
`VITE_ASSET_HOST`, so images come from `frontend/public` instead of the CDN.

Either half still runs on its own, if you would rather keep two shells:

```
cd backend && make api.watch.entr
cd frontend && npm run dev
```

`api.watch.entr` rather than `api.watch` because stack's `--file-watch` stopped
seeing edits under `library/` on this tree; `backend/Makefile` has the long
version of why, and every other watch target with it.

### Database

`scripts/setup` creates `arkham-horror-backend` from `setup.sql`, then applies
everything in `migrations/deploy` that the dump does not already contain,
recording what it ran in `arkham_schema_migrations`. By hand:

```
createdb arkham-horror-backend
psql -d arkham-horror-backend -f setup.sql
./scripts/setup --no-build      # applies the migrations on top
```

`migrations/sqitch.plan` and `migrations/sqitch.conf` are still in the tree but
sqitch is not what deploys any of this — the plan is read for the order of the
files in `migrations/deploy` and nothing else.

### Images

Image assets (~2.9 GB) are **not stored in the git repository**. They are hosted
on CloudFront and the app loads them from the CDN by default — no extra setup
needed. `scripts/boot` is the exception: it runs the frontends with
`VITE_ASSET_HOST` empty, which serves them out of `frontend/public`, so fetch
them if you boot that way and want to see art.

Fetching needs the `aws` CLI and `curl`:

```
make fetch-images     # Everything (~2.9 GB)
make fetch-cards      # English card images only (~755 MB)

# Or use the script directly:
./scripts/fetch-assets.sh en        # All English/static images (~1.3 GB)
./scripts/fetch-assets.sh en+fr     # English + French (recommended for French play)
./scripts/fetch-assets.sh en+es     # English + Spanish
./scripts/fetch-assets.sh en+ita    # English + Italian
./scripts/fetch-assets.sh en+ko     # English + Korean
./scripts/fetch-assets.sh en+zh     # English + Chinese
./scripts/fetch-assets.sh fr        # French card translations only
./scripts/fetch-assets.sh cards     # English + homebrew card images only (~755 MB)
./scripts/fetch-assets.sh 3ed       # Third edition images only (~106 MB)
```

If you only have Docker (no local AWS CLI), use the Docker-based targets instead:

```
make fetch-images-docker    # Everything via Docker (~2.9 GB)
make fetch-cards-docker     # English card images only via Docker

# Or run directly with the same targets as above:
docker compose --profile fetch-images run --rm fetch-images en
docker compose --profile fetch-images run --rm fetch-images en+fr
docker compose --profile fetch-images run --rm fetch-images fr
```

Running vite yourself, without `scripts/boot`, loads images from the CDN. To
point it at local copies anyway, create `frontend/.env.development.local`:
```
VITE_ASSET_HOST=
```

To go back to the CDN, remove the file or set
`VITE_ASSET_HOST=https://assets.arkhamhorror.app`.

If you add new images, sync them to S3 and regenerate the manifest:
```
make sync-and-manifest
```

To install a git hook that warns if you forget to update the manifest:
```
make install-hooks
```

### Offline builds

The self-contained packages at the top of this file are built by
`.github/workflows/build-offline.yml`, which runs on any `v*` tag and puts the
archives on a GitHub release. Cutting one:

```
make offline-build
```

That tags HEAD as `vYYYYMMDD.N` — picking the next free `N` for today — and
pushes the tag, which is the whole trigger. Two builds run, macOS ARM64 and
Linux x86_64; the other two platforms are in the matrix but commented out.
Each job is allowed two hours, which it needs — it compiles the Haskell and
builds PostgreSQL and nginx from source on top of that.

To build one on this machine instead, which is also how you debug the pipeline:

```
cd offline
./build_all.sh                  # --skip-deps, --clean and --verbose all work
```

It downloads its own GHC, Stack and Node rather than using yours, builds
PostgreSQL 14.15 and nginx from source so nothing carries a hard-coded path,
and leaves the archive in `offline/_dist/`. The toolchains and caches land in
`offline/_deps/` and `offline/_tmp/`, all three gitignored.

The build is not allowed to leave the source tree changed: it may patch the
frontend while it runs, but it puts it back. `offline/README.md` has the rest.

### Tests

```
cd backend && make test                     # the suite once
cd backend && make test.watch.entr          # on every change
cd backend/arkham-api && stack test --fast --test-arguments '-m "CardName"'
```

```
cd frontend && npm run tc                   # typecheck
cd frontend && npm test
```

## Homebrew content

Want to build your own campaign, scenario, or custom content? See
[Writing homebrew content](./docs/homebrew.md) — a hands-on guide to adding
cards, campaigns, traits, actions, tokens, and more without touching the base
game.

## Copyright Disclaimer

The information presented in this app about [Arkham Horror: The Card Game™][arkham], both textual and graphical, is © Fantasy Flight Games 2024. This app is a fan project and is not produced, endorsed, or supported by, or affiliated with Fantasy Flight Games.

All artwork and illustrations are the intellectual property of their respective creators. All Arkham Horror: The Card Game™ images and graphics are copyrighted by Fantasy Flight Games.

[arkham]: https://www.fantasyflightgames.com/en/products/arkham-horror-the-card-game/
[docker]: https://www.docker.com/
[wsl2]: https://learn.microsoft.com/en-us/windows/wsl/install
