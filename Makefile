PROJECT = occi
PROJECT_DESCRIPTION = OCCI Core implementation
PROJECT_VERSION = 0.1.0

DEPS = uri uuid mixer edown jsx

dep_uri = git https://github.com/erlware/uri.git 91f6b71
dep_uuid_commit = v1.5.1
dep_jsx_commit = 2.8.0

COMPILE_FIRST += occi_type occi_category occi_entity

EDOC_OPTS = {app_default, "http://www.erlang.org/doc/"} \
           ,{doclet, edown_doclet} \
           ,{top_level_readme, {"$(CURDIR)/doc/README.md", "http://github.com/erocci/erlang-occi"}}

include erlang.mk
