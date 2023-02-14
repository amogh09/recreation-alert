FROM haskell:9.2.4
RUN cabal update
COPY . $HOME/src
WORKDIR $HOME/src
#cabal --version
RUN cabal install
ENTRYPOINT "/bin/sh"
