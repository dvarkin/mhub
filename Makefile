PROJECT = mhub
PROJECT_DESCRIPTION = Simple message hubN
PROJECT_VERSION = 0.0.1

DEPS = ranch jiffy

DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

include erlang.mk
