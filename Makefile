PROJECT = occi
PROJECT_DESCRIPTION = OCCI Core implementation
PROJECT_VERSION = 0.0.1

DEPS = uri mixer fast_xml

dep_uri = git https://github.com/erlware/uri.git 91f6b71
dep_fast_xml = git https://github.com/processone/fast_xml.git 1.1.3
dep_jsx_commit = 2.8.0

include erlang.mk
