# NoKey

To start dev server:

  * `git clone ...`
  * Install dependencies with `mix deps.get`
  * Install js dependencies: `cd assets`, `yarn`, `cd ..`
  * Start Phoenix endpoint with `mix phx.server`

Now visit [`localhost:4000`](http://localhost:4000) from your browser.

## Deploy

  * Only get prod dependencies `mix deps.get --only prod`
  * generate a secret `mix phx.gen.secret` and keep it somewhere save

  * Possibly increment version in `mix.exs`
  * Compile `MIX_ENV=prod mix compile`
  * Build elm and assets:
    + (first time) `cd ../web/`, `elm-github-install`, `cd ../server`
    + `cd assets`, `yarn build`, `cd ..`
  * Digest assets `MIX_ENV=prod mix phx.digest`
  * (optional) test if it works: `PORT=4001 MIX_ENV=prod mix phx.server`
  * Build release:
    
    - if first time:
        + `MIX_ENV=prod mix release --env=prod`
        + (optional) test it: `REPLACE_OS_VARS=true PORT=4001 SECRET_KEY_BASE="<SECRET>" ERLANG_COOKIE="<SECRET>" _build/prod/rel/no_pass/bin/no_pass console`
        + Copy to server (TODO: TEST): `scp _build/prod/rel/no_pass/releases/0.0.1/no_pass.tar.gz tikstudent@virt35.ethz.ch:~/nokey/builds/`
            * (optional) to test locally: `cp _build/prod/rel/no_pass/releases/0.0.1/no_pass.tar.gz ~/Downloads/passwordManager/builds/`
        + (on server) unzip `tar xvf no_pass.tar.gz`
        + start `sudo REPLACE_OS_VARS=true PORT=80 SECRET_KEY_BASE='<SECRET>' ERLANG_COOKIE='<SECRET>' ./bin/no_pass start`
    
    - else (TODO: TEST):
        + create upgrade: `MIX_ENV=prod mix release --env=prod --upgrade`
        + copy to server: `scp _build/prod/rel/no_pass/releases/0.0.2/no_pass.tar.gz TODO/DEST/releases/0.0.2/`
        +  `./bin/no_pass upgrade 0.0.2`

