FROM haskell:9.2.4
RUN cabal update
COPY . $HOME/src
WORKDIR $HOME/src
RUN cabal install && cp -Lr ~/.cabal/bin/recreation-alert /recreation-alert

FROM alpine:latest
COPY --from=0 /recreation-alert /
ENTRYPOINT "/bin/sh"
