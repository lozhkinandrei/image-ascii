FROM haskell:9.2.4 as build

WORKDIR /opt/build
RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./image-ascii.cabal /opt/build/

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/build/
RUN cabal install

# Copy binary from the prebious stage and discard everything else
FROM debian:buster
WORKDIR /opt/image-ascii
COPY --from=build /root/.cabal/bin/image-ascii-exe  .

EXPOSE 3000
CMD ["./image-ascii-exe"]