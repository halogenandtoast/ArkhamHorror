# Setting up for development on Windows

## Backend

### Install Windows Subsystem for Linux (WSL) if you don't have it already

```
wsl --install
```

reboot

### Install Dependencies

In the wsl terminal, run the following commands:

```
sudo apt-get update
sudo apt-get install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 libz-dev libpq-dev postgresql
```


then install ghcup via the WSL way

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh


source the env file they tell you to and then install the correct ghc version, (currently 9.8.2) via `ghcup tui`,


### Try building the project

run `make api.watch` and wait a long time.


## Frontend

Make sure to have installed WSL as instructed above then, in a new terminal I ran:

```
sudo apt-get install nodejs npm
```


then upgrade the node version

```
sudo npm cache clean -f
sudo npm install -g n
sudo n stable
hash -r
```

then in the `frontend` directory:

```
npm i
npm run serve
```

## Database

Run `whoami` to check your username, if your username was `halogen64` for example, then run the following:

```
sudo -u postgres createuser -s halogen64
createdb arkham-horror-backend
psql arkham-horror-backend < setup.sql
```


Then to allow the backend to actually connect, you may need to edit `/etc/postgresql/14/main/pg_hba.conf` and changed all local connections from `peer` and `scram-sha-256` to `trust`

After that run the backend and frontend if not already running and you should be able to connect to http://localhost:8080/ and be able to create a new user and get started.
