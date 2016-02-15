PROJECT = occi
PROJECT_DESCRIPTION = OCCI Core implementation
PROJECT_VERSION = 0.0.1

DEPS = uri mixer annotations edown
DEP_PLUGINS = annotations

dep_uri = git https://github.com/erlware/uri.git 91f6b71
dep_jsx_commit = 2.8.0
dep_annotations_commit = 3f257db

COMPILE_FIRST += occi_category occi_entity

ANNOTATIONS = logging

EDOC_OPTS = {app_default, "http://www.erlang.org/doc/"} \
           ,{doclet, edown_doclet} \
           ,{top_level_readme, {"$(CURDIR)/README.md", "http://github.com/erocci/erlang-occi"}}

include erlang.mk
