FROM node:lts as frontend

# Frontend

ENV LC_ALL=C.UTF-8

RUN npm install --location=global vite

RUN mkdir -p /opt/arkham/src/frontend

WORKDIR /opt/arkham/src/frontend
COPY ./frontend/package.json ./frontend/tsconfig.json ./frontend/vite.config.js ./frontend/.eslintrc.cjs ./frontend/package-lock.json /opt/arkham/src/frontend/
RUN npm ci
COPY ./frontend /opt/arkham/src/frontend
ENV VITE_ASSET_HOST ${ASSET_HOST:-""}
RUN npm run build

FROM ubuntu:22.04 as base

ARG DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C.UTF-8
ENV TZ=UTC

# install dependencies
RUN \
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends --fix-missing \
        libpq-dev \
        curl \
        libtinfo6 \
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
  rm -rf /var/lib/apt/lists/*

ARG TARGETARCH

# install ghcup
RUN \
    if [ "$TARGETARCH" = "arm64" ]; then \
    curl https://downloads.haskell.org/~ghcup/aarch64-linux-ghcup > /usr/bin/ghcup; \
    else \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup; \
    fi;
# Don't combine
RUN chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGNone

ARG GHC=9.8.2
ARG CABAL=3.10.3.0
ARG STACK=2.15.7
ARG CACHE_ID="${TARGETARCH}-${GHC}-${CABAL}-${STACK}"
ENV CACHE_ID=${CACHE_ID}
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1

# install GHC and cabal
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} && \
    ghcup -v install stack --isolate /usr/local/bin --force ${STACK}

FROM base as dependencies

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
RUN --mount=type=cache,id=stack-root-${CACHE_ID},target=/opt/arkham/src/backend/.stack-work \
    --mount=type=cache,id=stack-api-${CACHE_ID},target=/opt/arkham/src/backend/arkham-api/.stack-work \
    --mount=type=cache,id=stack-api-hie-${CACHE_ID},target=/opt/arkham/src/backend/arkham-api/.hie \
    --mount=type=cache,id=stack-core-${CACHE_ID},target=/opt/arkham/src/backend/arkham-core/.stack-work \
    --mount=type=cache,id=stack-core-hie-${CACHE_ID},target=/opt/arkham/src/backend/arkham-core/.hie \
    --mount=type=cache,id=stack-validate-hie-${CACHE_ID},target=/opt/arkham/src/backend/arkham-validate/.hie \
    --mount=type=cache,id=stack-discover-hie-${CACHE_ID},target=/opt/arkham/src/backend/cards-discover/.hie \
    stack build --system-ghc --dependencies-only --no-terminal --ghc-options '-j4 +RTS -A128m -n2m -RTS'

FROM dependencies as api

RUN mkdir -p \
  /opt/arkham/src/backend \
  /opt/arkham/bin

COPY ./backend /opt/arkham/src/backend

WORKDIR /opt/arkham/src/backend/cards-discover
RUN --mount=type=cache,id=stack-discover-${CACHE_ID},target=/opt/arkham/src/backend/cards-discover/.stack-work \
    stack build --system-ghc --no-terminal --ghc-options '-j4 +RTS -A128m -n2m -RTS' cards-discover

WORKDIR /opt/arkham/src/backend/arkham-api
RUN --mount=type=cache,id=stack-root-${CACHE_ID},target=/opt/arkham/src/backend/.stack-work \
    --mount=type=cache,id=stack-api-${CACHE_ID},target=/opt/arkham/src/backend/arkham-api/.stack-work \
    --mount=type=cache,id=stack-api-hie-${CACHE_ID},target=/opt/arkham/src/backend/arkham-api/.hie \
    --mount=type=cache,id=stack-core-${CACHE_ID},target=/opt/arkham/src/backend/arkham-core/.stack-work \
    --mount=type=cache,id=stack-core-hie-${CACHE_ID},target=/opt/arkham/src/backend/arkham-core/.hie \
    --mount=type=cache,id=stack-validate-hie-${CACHE_ID},target=/opt/arkham/src/backend/arkham-validate/.hie \
    --mount=type=cache,id=stack-discover-hie-${CACHE_ID},target=/opt/arkham/src/backend/cards-discover/.hie \
  stack build --no-terminal --system-ghc --ghc-options '-j4 +RTS -A128m -n2m -RTS' && \
  stack --no-terminal --local-bin-path /opt/arkham/bin install

FROM ubuntu:22.04 as app

# App

ENV LC_ALL=C.UTF-8

RUN apt-get update && \
  apt-get upgrade -y --assume-yes && \
  apt-get install -y --assume-yes libpq-dev ca-certificates nginx curl && \
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
COPY ./backend/arkham-api/digital-ocean.crt /opt/arkham/src/backend/arkham-api/digital-ocean.crt

RUN useradd -ms /bin/bash yesod && \
  chown -R yesod:yesod /opt/arkham /var/log/nginx /var/lib/nginx /run && \
  chmod a+x /opt/arkham/src/backend/arkham-api/start.sh
USER yesod
ENV PATH "$PATH:/opt/stack/bin:/opt/arkham/bin"

EXPOSE 3000

WORKDIR /opt/arkham/src/backend/arkham-api
CMD ["./start.sh"]
