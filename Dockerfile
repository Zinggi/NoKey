# ---- Build Stage ----
FROM elixir:1.11-slim AS builder

RUN apt-get update && apt-get install -y ca-certificates curl gnupg git \
    && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
    && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
    && apt-get update && apt-get install -y nodejs npm yarn

WORKDIR /work
ENV MIX_ENV=prod
RUN mix local.rebar --force \
    && mix local.hex --force


# deps
COPY server/mix.exs server/mix.lock ./server/
RUN cd server && mix deps.get && mix compile

# frontend deps
COPY web/package.json web/yarn.lock web/elm-package.json ./web/
COPY server/assets/package.json server/assets/yarn.lock ./server/assets/
RUN yarn global add elm-github-install \
    && cd web && elm-github-install && yarn && cd .. \
    && cd server/assets && yarn


# build frontend
COPY web ./web
COPY server ./server

RUN cd server/assets && yarn build
RUN cd server/ && MIX_ENV=prod mix phx.digest \
    && MIX_ENV=prod mix compile \
    && mix release

# ---- Application Stage ----
FROM elixir:1.11-slim

WORKDIR /app

RUN groupadd --gid 61000 nobody \
    && chown nobody:nobody /app
USER nobody:nobody

COPY --from=builder --chown=nobody:nobody /work/server/_build/prod/rel/no_pass ./
ENV HOME=/app

CMD ["./bin/no_pass", "start"]
