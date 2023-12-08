FROM haskell:9.6

WORKDIR /app
COPY . /app

RUN apt update && apt-get install -y build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 llvm