PROJECT = occi
PROJECT_DESCRIPTION = OCCI Core implementation
PROJECT_VERSION = 0.0.1

DEPS = uri mixer annotations
DEP_PLUGINS = annotations

dep_uri = git https://github.com/erlware/uri.git 91f6b71
dep_jsx_commit = 2.8.0
dep_annotations_commit = 3f257db

ANNOTATIONS = logging

include erlang.mk
