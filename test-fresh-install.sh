#! /bin/bash
#

ORG_DEPEND_VERSION="0.0.1"

ROOT=.
DIST=dist
TMP_HOME=/tmp/home-org-depend

cask package ${ROOT}/${DIST}
HOME=$TMP_HOME cask emacs --eval \
"(package-install-file \"${ROOT}/${DIST}/org-depend-${ORG_DEPEND_VERSION}.tar\")"
