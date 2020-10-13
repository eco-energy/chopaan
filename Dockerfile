FROM fpco/stack-build:lts-14.17 as build

RUN mkdir /opt/build
COPY . /opt/build

VOLUME /tmp/stackroot

RUN mkdir /opt/build/binaries

RUN cd /opt/build && stack --stack-root=/tmp/stackroot build --system-ghc --copy-bins --local-bin-path=/opt/build/binaries

FROM fpco/pid1
RUN mkdir -p /opt/app
ARG BINARY_PATH
WORKDIR /opt/app

RUN apt-get update && apt-get install -y ca-certificates libgmp-dev

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY --from=build /opt/build/binaries .

CMD ["/opt/app/chopaan-exe"]
