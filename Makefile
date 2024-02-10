# With this Makefile you can build and collect the entire project under a single
# directory ( /ebin ).
#
# You can select specific projects by calling make with the SELECT argument:
# make SELECT=bencode,polycache

ROOT_DIR := $(realpath $(dir $(realpath $(lastword $(MAKEFILE_LIST)))))

EBIN := ${ROOT_DIR}/ebin


ifdef SELECT
	SEP := ,
    _SELECT_ := $(subst ${SEP}, ,${SELECT})
    APPS := $(sort $(addprefix ${ROOT_DIR}/,${_SELECT_}))
else
    APPS := $(sort $(dir $(wildcard ${ROOT_DIR}/*/)))
endif

APPS_EBIN := $(addsuffix ebin,${APPS})


.PHONY: all clean


all: ${APPS}
	mkdir -p ${EBIN}
	rm -rf ${EBIN}/*

	@for app in $^; do                                 \
        cd $$app && $(MAKE) && cp ./ebin/* ../ebin/;   \
    done


clean: ${APPS}
	@for app in $^; do               \
        cd $$app && $(MAKE) clean;   \
    done

	rm -rf ${EBIN}
