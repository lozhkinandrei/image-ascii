FROM haskell:9.2.4

WORKDIR /opt/image-ascii
RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./image-ascii.cabal /opt/image-ascii/

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/image-ascii/
RUN cabal install

EXPOSE 3000
CMD ["image-ascii-exe"]