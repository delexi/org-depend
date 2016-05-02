#! /bin/bash
#

ORG_DPNDS_VERSION="0.0.1"

ROOT=.
DIST=dist
TMP_HOME=/tmp/home-org-dpnds

if [[ ! -d ${TMP_HOME} ]]; then
    mkdir ${TMP_HOME}
fi

cask install
cask package

HOME=$TMP_HOME cask emacs --eval \
"(package-install-file \"${ROOT}/${DIST}/org-dpnds-${ORG_DPNDS_VERSION}.tar\")"
