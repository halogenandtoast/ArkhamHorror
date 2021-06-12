FROM heroku/heroku:18 as frontend

# Frontend

ENV LC_ALL=en_US.UTF-8

RUN apt-get update
RUN apt-get upgrade -y --assume-yes

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt update && apt-get install -y --assume-yes yarn

RUN yarn global add @vue/cli

RUN mkdir -p /opt/arkham/src/frontend

# ENV VUE_APP_API_HOST "https://arkham-horror-api.herokuapp.com"

WORKDIR /opt/arkham/src/frontend
COPY ./frontend/package.json /opt/arkham/src/frontend/package.json
COPY ./frontend/babel.config.js /opt/arkham/src/frontend/babel.config.js
COPY ./frontend/tsconfig.json /opt/arkham/src/frontend/tsconfig.json
COPY ./frontend/yarn.lock /opt/arkham/src/frontend/yarn.lock
RUN yarn install --frozen-lockfile
WORKDIR /opt/arkham/src/frontend
COPY ./frontend /opt/arkham/src/frontend
RUN yarn build

FROM heroku/heroku:18 as dependencies

# Yesod

ENV LC_ALL=en_US.UTF-8

RUN apt-get update
RUN apt-get upgrade -y --assume-yes
RUN apt-get install -y --assume-yes g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg
RUN apt-get install -y --assume-yes libpq-dev

RUN rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

ENV PATH "$PATH:/opt/stack/bin:/opt/arkham/bin"

RUN mkdir -p /opt/arkham/src
RUN mkdir -p /opt/arkham/bin
RUN mkdir -p /opt/arkham/src/backend/arkham-api
RUN mkdir -p /opt/arkham/src/backend/arkham-core
RUN mkdir -p /opt/arkham/src/frontend

WORKDIR /opt/arkham/src/backend
COPY ./backend/stack.yaml /opt/arkham/src/backend/stack.yaml
COPY ./backend/arkham-api/package.yaml /opt/arkham/src/backend/arkham-api/package.yaml
COPY ./backend/arkham-core/package.yaml /opt/arkham/src/backend/arkham-core/package.yaml
RUN stack --no-terminal setup

WORKDIR /opt/arkham/src/backend/arkham-api
RUN sed -i '/arkham-core/d' package.yaml
RUN stack install yesod-bin --install-ghc --ghc-options '-j4 +RTS -A64m -n2m -RTS'
RUN stack --no-terminal test --only-dependencies --ghc-options '-j4 +RTS -A64m -n2m -RTS'

FROM heroku/heroku:18 as api

# API

ENV LC_ALL=en_US.UTF-8

RUN apt-get update
RUN apt-get upgrade -y --assume-yes
RUN apt-get install -y --assume-yes g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg
RUN apt-get install -y --assume-yes libpq-dev

RUN rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

ENV PATH "$PATH:/opt/stack/bin:/opt/arkham/bin"

RUN mkdir -p /opt/arkham/src/backend
RUN mkdir -p /opt/arkham/src/frontend
RUN mkdir -p /opt/arkham/bin

COPY ./backend /opt/arkham/src/backend
COPY --from=frontend /opt/arkham/src/frontend/dist /opt/arkham/src/frontend/dist
COPY --from=dependencies /root/.stack /root/.stack

WORKDIR /opt/arkham/src/backend/arkham-api
RUN stack --no-terminal build --ghc-options '-j4 +RTS -A64m -n2m -RTS'
RUN stack --no-terminal --local-bin-path /opt/arkham/bin install

FROM heroku/heroku:18

# App

ENV LC_ALL=en_US.UTF-8

RUN mkdir -p /opt/arkham/bin
RUN mkdir -p /opt/arkham/src/backend/arkham-api
RUN mkdir -p /opt/arkham/src/frontend

COPY --from=frontend /opt/arkham/src/frontend/dist /opt/arkham/src/frontend/dist
COPY --from=api /opt/arkham/bin/arkham-api /opt/arkham/bin/arkham-api
COPY ./backend/arkham-api/config /opt/arkham/src/backend/arkham-api/config

RUN useradd -ms /bin/bash yesod
RUN chown -R yesod:yesod /opt/arkham
USER yesod
ENV PATH "$PATH:/opt/stack/bin:/opt/arkham/bin"

EXPOSE 3000

WORKDIR /opt/arkham/src/backend/arkham-api
CMD ["/opt/arkham/bin/arkham-api"]
