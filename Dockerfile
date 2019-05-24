FROM fpco/stack-build:lts-12.26 as build

RUN mkdir -p /app/build
COPY . /app/build
RUN cd /app/build && stack build --system-ghc
FROM ubuntu:18.04

RUN apt update
RUN apt-get install -y postgresql postgresql-contrib

RUN mkdir -p /app/run
# COPY --from=build /app/build/selective-shop /app/run/run-server
COPY --from=build /app/build/.stack-work/install/x86_64-linux/lts-12.26/8.4.4/bin/selective-shop /app/run/run-server
COPY static /app/run/static

WORKDIR /app/run
EXPOSE 3000

CMD ./run-server
