# Developing on Windows

Everything runs inside WSL. The build leans on a Unix shell throughout — the
Makefile targets, the watch loops, `scripts/setup` and `scripts/boot` — so
building against Windows directly is not a path anyone here keeps working.
Once WSL is up the work is the same as it is on Linux, so most of this page is
getting WSL ready and then handing over to `scripts/setup`.

If you only want to play the game rather than work on it, you do not need any
of this: see [Installing with Docker on Windows](./install/docker-windows.md)
or the [prebuilt package](./install/prebuilt.md), which is the easier of the
two.

## WSL

```
wsl --install
```

Reboot, then open the Ubuntu terminal. Everything below happens in there, and
the checkout should live in the WSL filesystem (`~/ArkhamHorror`, say) rather
than under `/mnt/c`. A build on `/mnt/c` is painfully slow, and Postgres will
not accept a data directory there at all.

## System packages

GHC and the Haskell packages that bind to C libraries need these:

```
sudo apt-get update
sudo apt-get install build-essential curl git pkg-config \
  libffi-dev libgmp-dev libncurses-dev zlib1g-dev libpq-dev postgresql
```

## Postgres

WSL does not start services for you unless you have enabled systemd, so start
it by hand and make yourself a superuser:

```
sudo service postgresql start
sudo -u postgres createuser -s "$(whoami)"
```

`scripts/setup` creates the database itself, so there is nothing else to do
here. If it cannot connect, Postgres is probably still set to `peer` or
`scram-sha-256` for local connections — find the config with

```
sudo -u postgres psql -tAc 'show hba_file'
```

and change the local lines to `trust`, then `sudo service postgresql reload`.

Postgres does not survive a reboot of WSL, so `sudo service postgresql start`
is a thing you will type again.

## The rest

From the checkout:

```
./scripts/setup
```

It installs ghcup, the GHC that `backend/stack.yaml` pins (9.14.1 today),
Stack and a current Node, then the node modules, the database and a first
build of the backend. `./scripts/setup --check` lists what is missing without
touching anything.

Then:

```
./scripts/boot
```

and open http://localhost:8080 in a Windows browser. WSL forwards localhost,
so the frontend on 8080 and the API on 3002 usually need nothing extra. If the
page will not load, vite has probably bound to `::1` alone, which the
forwarding does not always pick up — `npm run dev -- --host` in `frontend/`
binds everything and settles it.

The [Local dev](../README.md#local-dev) section of the README covers what the
two scripts do and how to run either half on its own.
