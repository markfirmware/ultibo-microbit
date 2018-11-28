#!/bin/bash
set -e

LAZDIR="$HOME/ultibo/core"

LPINAME=$1
MODE=$2

echo $MODE
rm -rf lib/
WD=$(pwd)
pushd $LAZDIR >& /dev/null
./lazbuild --build-mode=$MODE $WD/$LPINAME.lpi
popd >& /dev/null
mv kernel* $LPINAME-kernel-${MODE,,}.img
