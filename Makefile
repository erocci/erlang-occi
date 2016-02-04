PROJECT = occi
PROJECT_DESCRIPTION = OCCI Core implementation
PROJECT_VERSION = 0.0.1

DEPS = uri

dep_uri = git https://github.com/erlware/uri.git 91f6b71

include erlang.mk
