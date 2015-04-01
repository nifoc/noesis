PROJECT = noesis
PROJECT_VERSION = 0.3

otp_release = $(shell erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), init:stop()')
otp_17plus = $(shell echo $(otp_release) | grep -q -E "^[[:digit:]]+$$" ; echo $$?)

TEST_DEPS = triq nifoc_ct_helper
dep_triq = git https://github.com/krestenkrab/triq master
dep_nifoc_ct_helper = git https://github.com/nifoc/nifoc_ct_helper master

ifeq ($(USER),travis)
	TEST_DEPS += ecoveralls
	dep_ecoveralls = git https://github.com/nifoc/ecoveralls master
endif

ERLC_OPTS ?= -Werror +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

TEST_ERLC_OPTS ?= +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

ifeq ($(otp_17plus),0)
	ERLC_OPTS += -Dnamespaced_types=1
	TEST_ERLC_OPTS += -Dnamespaced_types=1
endif

CT_SUITES = eunit triq
CT_OPTS = -ct_hooks nifoc_ct_hook [] -cover ./test/cover.spec

EDOC_OPTS = {def, [ \
					{years, "2014-2015"}, \
					{version, "$(PROJECT_VERSION)"} \
				]}

include erlang.mk

deps/horse:
	git clone -n -- https://github.com/extend/horse $(DEPS_DIR)/horse
	cd $(DEPS_DIR)/horse ; git checkout -q master
	$(MAKE) -C $(DEPS_DIR)/horse

perfs: ERLC_OPTS += -DPERF=1 +'{parse_transform, horse_autoexport}'
perfs: clean deps deps/horse app
	$(gen_verbose) erl -noshell -pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'

coverage-report: $(shell ls -1rt `find logs -type f -name \*.coverdata 2>/dev/null` | tail -n1)
	$(gen_verbose) erl -noshell -pa ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

upload-docs: docs
	$(gen_verbose) rsync -avz --no-o --no-g -e ssh --chmod=og=r -p --delete --exclude '*.edoc' --exclude 'edoc-info' doc/ webserver.kempkens.io:/var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)
	@ssh webserver.kempkens.io chown -R www:www /var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)

.PHONY: coverage-report upload-docs
