# Arkham Horror LCG

![Screenshot](./docs/img/screenshot.png)

The goal of this project is to implement a web version of Arkham Horror with as
many of the rules implemented as possible.

## Warning

This is very much a work in progress. Things may break at any time, but if they do,
please file a bug.

## Features

* Multiplayer up to 4 players
* Multiplayer solitaire
* Tarot Readings
* Deck import from ArkhamDB and arkham.build

## Finished Content

### Player Cards

* All player cards before 2026

### Campaigns

* Night of the Zealot
  * Return to Night of the Zealot
* The Dunwich Legacy
  * Return to The Dunwich Legacy
* The Path To Carcosa
  * Return to The Path To Carcosa
* The Forgotten Age
  * Return to The Forgotten Age
* The Circle Undone
  * Return to The Circle Undone
* The Dream-Eaters
* The Innsmouth Conspiracy
* Edge of the Earth

### Side Stories

* The Curse of the Rougarou
* Carnevale of Horrors
* Murder at the Excelsior Hotel
* The Midwinter Gala
* Film Fatale

## I just want to try this out on my computer

You will need to install [Docker][docker].

Afterwards you can run:

```
docker compose up
```

And launch http://localhost:3000

If you pull updates in the future make sure to run

```
docker compose down
docker compose build
docker compose up
```

## Local dev

### Dependencies

* Stack for GHC
* Node
* Postgresql
* Sqitch (optional: for migrations)

### Local Setup

#### Running via Docker

The image is setup to use an external database passed via the `DATABASE_URL` environment variable. Follow the steps below to setup the database and then run the following commands

```
docker build -t arkham .
docker run -t -i -e PORT=3000 -e DATABASE_URL="postgres://docker:docker@host.docker.internal:5432/arkham-horror-backend" -p 3000:3000 arkham
```

#### Backend

Run `stack setup` in the `backend` directory, then run `stack build --fast` (note: this will still take a long time)

#### Frontend

Run `npm install` in the `frontend` directory

#### Images

Image assets (~2.9 GB) are **not stored in the git repository**. They are hosted
on CloudFront and the app loads them from the CDN by default in both development
and production — no extra setup needed.

If you need local copies (e.g. for offline development), the fetch script
downloads directly from the public CDN using `curl` (no AWS credentials needed):

```
make fetch-images     # Download all images (~2.9 GB)
make fetch-cards      # Download only English card images (~755 MB)

# Or use the script directly for specific languages:
./scripts/fetch-assets.sh fr    # French only
./scripts/fetch-assets.sh en    # All English images
```

To use local images instead of CDN, create `frontend/.env.development.local`:
```
VITE_ASSET_HOST=
```

If you add new images, sync them to S3 and regenerate the manifest:
```
make sync-and-manifest
```

To install a git hook that warns if you forget to update the manifest:
```
make install-hooks
```

#### Database
Create the local database:

```
createdb arkham-horror-backend
cd migrations
sqitch deploy db:pg:arkham-horror-backend
```

If you do not have sqitch you can `cat migrations/deploy/*` to see the create
table statements and run them manually, you will want to specifically run the
`users` and `arkham_games` create table statements first.

### Running the server

* start the backend with `cd backend && make api.watch`
* start the frontend with `cd frontend && npm run serve`

## Copyright Disclaimer

The information presented in this app about [Arkham Horror: The Card Game™][arkham], both textual and graphical, is © Fantasy Flight Games 2024. This app is a fan project and is not produced, endorsed, or supported by, or affiliated with Fantasy Flight Games.

All artwork and illustrations are the intellectual property of their respective creators. All Arkham Horror: The Card Game™ images and graphics are copyrighted by Fantasy Flight Games.

[arkham]: https://www.fantasyflightgames.com/en/products/arkham-horror-the-card-game/
[docker]: https://www.docker.com/
