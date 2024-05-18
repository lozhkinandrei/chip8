FROM haskell:9.8.2

WORKDIR /opt/chip

RUN apt-get update && apt-get install libsdl2-dev -y

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./chip.cabal /opt/chip/

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/chip/
RUN cabal install