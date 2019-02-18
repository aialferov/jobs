ARG PROJECT=
ARG ERLANG_VERSION=

FROM aialferov/erlang:"${ERLANG_VERSION}" AS builder
LABEL project="${PROJECT}"

COPY . src
RUN make -C src package-ready DESTDIR=/build

FROM alpine:3.9
LABEL project="${PROJECT}"
RUN apk add --no-cache --update ncurses

COPY --from=builder /build /

ENTRYPOINT ["/usr/local/bin/jose"]
