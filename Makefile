PROJECT = enexmo
PROJECT_DESCRIPTION = Nexmo REST API client for Erlang
PROJECT_VERSION = 1.0.0

LOCAL_DEPS = ssl inets
DEPS = jsx

dep_jsx = git https://github.com/talentdeficit/jsx.git 2.7.1

include erlang.mk
