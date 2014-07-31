PROJECT = noesis

otp_17plus = $(shell erl -noshell -noinput -eval 'io:format("~s", [erlang:system_info(otp_release)]), init:stop().' | \
				grep -q -E "^\d+$$" ; echo $$?)

ERLC_OPTS ?= -Werror +debug_info +warn_bif_clash +warn_deprecated_function +warn_export_all \
				+warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

ifeq ($(otp_17plus),0)
ERLC_OPTS += -Dnamespaced_types=1
endif

include erlang.mk
