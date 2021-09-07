# Arkham Horror LCG

The goal of this project is to implement a web version of Arkham Horror with as
many of the rules implemented as possible.

##

## Features

* Multiplayer up to 4 players
* Multiplayer solitaire
* Deck import from ArkhamDB

## Finished Content

### Player Cards

* All player cards up to the Carcosa deluxe box

### Campaigns

* Night of the Zealot
* The Dunwich Legacy
* The Path To Carcosa [Curtain Call]

### Side Stories

* The Curse of the Rougarou
* Carnevale of Horrors

## Local dev

### Dependencies

* Stack for GHC
* Node
* Postgresql
* Nginx (for local development)
* Sqitch (optional: for migrations)

### Local Setup

#### Backend

Run `stack setup` in the `backend` directory, then run `stack build --fast` (note: this will still take a long time)

#### Frontend

Run `npm install` in the `frontend` directory

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
* start the frontend with `cd frontend && PORT=8081 npm run serve`
* start the ngingx server with ``nginx -c `pwd`/local.nginxconf``
