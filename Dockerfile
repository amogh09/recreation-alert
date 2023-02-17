FROM haskell:9.2.4-slim
RUN cabal update
COPY ./*.cabal $HOME/src/
WORKDIR $HOME/src
RUN cabal build --only-dependencies

COPY . $HOME/src/
RUN cabal install --installdir=. --install-method=copy --overwrite-policy=always

FROM debian:stable-slim
COPY --from=0 $HOME/src/recreation-alert /
ENTRYPOINT ["/recreation-alert"]
