FROM node:lts as frontend

# Frontend

ENV LC_ALL=en_US.UTF-8

RUN npm install --location=global @vue/cli

RUN mkdir -p /opt/arkham/src/frontend

# ENV VUE_APP_API_HOST "https://arkham-horror-api.herokuapp.com"

WORKDIR /opt/arkham/src/frontend
COPY ./frontend/package.json /opt/arkham/src/frontend/package.json
COPY ./frontend/babel.config.js /opt/arkham/src/frontend/babel.config.js
COPY ./frontend/tsconfig.json /opt/arkham/src/frontend/tsconfig.json
COPY ./frontend/package-lock.json /opt/arkham/src/frontend/package-lock.json
RUN npm ci
WORKDIR /opt/arkham/src/frontend
COPY ./frontend /opt/arkham/src/frontend
ENV VUE_APP_ASSET_HOST ${ASSET_HOST:-""}
RUN npm run build

FROM ubuntu:18.04 as dependencies

ARG DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=en_US.UTF-8
ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# install dependencies
RUN \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
        libpq-dev \
        postgresql \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        libtinfo-dev \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential && \
  apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/3bf863cc.pub && \
  rm -rf /var/lib/apt/lists/*

# install gpg keys
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGStrict

ARG GHC=9.4.2
ARG CABAL=latest
ARG STACK=latest

# install GHC and cabal
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} && \
    ghcup -v install stack --isolate /usr/local/bin --force ${STACK}

RUN mkdir -p \
  /opt/arkham/bin \
  /opt/arkham/src/backend/arkham-api \
  /opt/arkham/src/backend/arkham-core \
  /opt/arkham/src/backend/validate \
  /opt/arkham/src/backend/cards-discover

WORKDIR /opt/arkham/src/backend
COPY ./backend/stack.yaml /opt/arkham/src/backend/stack.yaml
COPY ./backend/arkham-api/package.yaml /opt/arkham/src/backend/arkham-api/package.yaml
COPY ./backend/arkham-core/package.yaml /opt/arkham/src/backend/arkham-core/package.yaml
COPY ./backend/validate/package.yaml /opt/arkham/src/backend/validate/package.yaml
COPY ./backend/cards-discover/package.yaml /opt/arkham/src/backend/cards-discover/package.yaml
RUN stack build --system-ghc --dependencies-only --no-terminal --ghc-options '-j4 +RTS -A128m -n2m -RTS'

FROM ubuntu:18.04 as api

ARG DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=en_US.UTF-8
ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# install dependencies
RUN \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
        libpq-dev \
        postgresql \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        libtinfo-dev \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential && \
  apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/3bf863cc.pub && \
  rm -rf /var/lib/apt/lists/*

# install gpg keys
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGStrict

ARG GHC=9.4.2
ARG CABAL=latest
ARG STACK=latest

# install GHC and cabal
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} && \
    ghcup -v install stack --isolate /usr/local/bin --force ${STACK}

RUN mkdir -p \
  /opt/arkham/src/backend \
  /opt/arkham/bin

COPY ./backend /opt/arkham/src/backend
COPY --from=dependencies /root/.stack /root/.stack

WORKDIR /opt/arkham/src/backend/cards-discover
RUN stack build --system-ghc --no-terminal --ghc-options '-j4 +RTS -A128m -n2m -RTS' cards-discover

WORKDIR /opt/arkham/src/backend/arkham-api
RUN stack build --no-terminal --system-ghc --ghc-options '-j4 +RTS -A128m -n2m -RTS'
RUN stack --no-terminal --local-bin-path /opt/arkham/bin install

FROM ubuntu:18.04 as app

# App

ENV LC_ALL=en_US.UTF-8

RUN apt-get update && \
  apt-get upgrade -y --assume-yes && \
  apt-get install -y --assume-yes libpq-dev ca-certificates nginx && \
  rm -rf /var/lib/apt/lists/*

RUN mkdir -p \
  /opt/arkham/bin \
  /opt/arkham/src/backend/arkham-api \
  /opt/arkham/src/frontend \
  /var/log/nginx \
  /var/lib/nginx \
  /run

COPY --from=frontend /opt/arkham/src/frontend/dist /opt/arkham/src/frontend/dist
COPY --from=api /opt/arkham/bin/arkham-api /opt/arkham/bin/arkham-api
COPY ./backend/arkham-api/config /opt/arkham/src/backend/arkham-api/config
COPY ./prod.nginxconf /opt/arkham/src/backend/prod.nginxconf
COPY ./start.sh /opt/arkham/src/backend/arkham-api/start.sh

RUN useradd -ms /bin/bash yesod && \
  chown -R yesod:yesod /opt/arkham /var/log/nginx /var/lib/nginx /run && \
  chmod a+x /opt/arkham/src/backend/arkham-api/start.sh
USER yesod
ENV PATH "$PATH:/opt/stack/bin:/opt/arkham/bin"

EXPOSE 3000

WORKDIR /opt/arkham/src/backend/arkham-api
CMD ["./start.sh"]
