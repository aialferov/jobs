ARG ERLANG_VERSION=21.2
FROM aialferov/erlang:$ERLANG_VERSION AS builder
LABEL project=jose

COPY . src
RUN make -C src package-ready DESTDIR=/build

FROM alpine:3.9
LABEL project=jose
RUN apk add --no-cache --update ncurses

COPY --from=builder /build /

ENTRYPOINT ["/usr/local/bin/jose"]
