# NoKey

To start dev server:

  * `git clone ...`
  * Install dependencies with `mix deps.get`
  * Install js dependencies: `cd assets`, `yarn`, `cd ..`
  * Start Phoenix endpoint with `mix phx.server`

Now visit [`localhost:4000`](http://localhost:4000) from your browser.


## Let's encrypt guide

  * mostly follow this: https://medium.com/@a4word/phoenix-app-secured-with-let-s-encrypt-469ac0995775
  * `vim /etc/letsencrypt/letsencrypt.ini`
  * make sure webroot is correct

## Deploy

  * Only get prod dependencies `mix deps.get --only prod`
  * generate a secret `mix phx.gen.secret` and keep it somewhere save

  * Build elm and assets:
    + (first time) `cd ../web/`, `elm-github-install`, `cd ../server`
    + `cd assets`, `yarn build`, `cd ..`
  * Digest assets `MIX_ENV=prod mix phx.digest`
  * Possibly increment version in `server/mix.exs`
  * Compile `MIX_ENV=prod mix compile`
  * (optional) test if it works: `sudo PORT=4001 SSL_PORT=4443 MIX_ENV=prod mix phx.server`
    + If there is an error, maybe cleaning elm-stuff helps? `rm -r elm-stuff/build-artifacts/*`
  * Build release:
    
    - if first time:
        + `MIX_ENV=prod mix release --env=prod`
        + (optional) test it: `REPLACE_OS_VARS=true PORT=4001 SSL_PORT=4443 SECRET_KEY_BASE="<SECRET>" ERLANG_COOKIE="<SECRET>" _build/prod/rel/no_pass/bin/no_pass console`
        + Copy release: `cp _build/prod/rel/no_pass/releases/0.0.1/no_pass.tar.gz ~/nokey/builds/`
        + `cd ~/nokey/builds/`
        + unzip `tar xvf no_pass.tar.gz`
        + start `sudo REPLACE_OS_VARS=true PORT=80 SSL_PORT=443 SECRET_KEY_BASE='<SECRET>' ERLANG_COOKIE='<SECRET>' ./bin/no_pass start`
    
    - else:
        + create upgrade: `MIX_ENV=prod mix release --env=prod --upgrade`
        + create folder: `mkdir ~/nokey/builds/releases/<0.0.2>`
        + copy: `cp _build/prod/rel/no_pass/releases/<0.0.2>/no_pass.tar.gz ~/nokey/builds/releases/<0.0.2>/`
        + `cd ~/nokey/builds/`
        + `<SAME_ENVS_USED_TO_START> ~/nokey/builds/bin/no_pass upgrade <0.0.2>`
            (HINT: use ctrl+r to search your start command in the history)

