PROJECT = occi
PROJECT_DESCRIPTION = OCCI Core implementation
PROJECT_VERSION = 0.0.1

DEPS = uri uuid mixer annotations edown
DEP_PLUGINS = annotations

dep_uri = git https://github.com/erlware/uri.git 91f6b71
dep_uuid_commit = v1.5.1
dep_jsx_commit = 2.8.0
dep_annotations_commit = 9f8a800

COMPILE_FIRST += occi_type occi_category occi_entity

ANNOTATIONS = logging

EDOC_OPTS = {app_default, "http://www.erlang.org/doc/"} \
           ,{doclet, edown_doclet} \
           ,{top_level_readme, {"$(CURDIR)/doc/README.md", "http://github.com/erocci/erlang-occi"}}

include erlang.mk
