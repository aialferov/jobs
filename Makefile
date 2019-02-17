include mk/Erlangbin.mk
include mk/Docker.mk

USER = aialferov
PORT = 8080

RUN_ARGS = run

DOCKER_RUN_ARGS_EXTRA = \
    -p $(PORT):$(PORT)

ifdef ERLANG_VERSION
    DOCKER_BUILD_ARGS_EXTRA = \
        --build-arg ERLANG_VERSION=$(ERLANG_VERSION)
endif
