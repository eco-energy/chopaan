FROM fpco/stack-build:lts-14.17 as build

RUN mkdir /opt/build
COPY . /opt/build

VOLUME /tmp/stackroot

RUN cd /opt/build && stack --stack-root=/tmp/stackroot build --system-ghc

FROM fpco/pid1
RUN mkdir -p /opt/app
ARG BINARY_PATH
WORKDIR /opt/app

RUN apt-get update && apt-get install -y ca-certificates libgmp-dev

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-14.17/8.6.5/bin .

CMD ["/opt/app/chopaan"]
