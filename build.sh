#!/bin/bash

shopt -s nullglob

if [ -d "/c/Ultibo/Core" ]; then
	LAZDIR=/c/Ultibo/Core
fi

if [ -d "$HOME/ultibo/core" ]; then
	LAZDIR="$HOME/ultibo/core"
fi

MAKE_KERNELS=0

function buildLpi {
    LPINAME="$1"
    MODE="unknown"
    grep '<TargetController Value="QEMUVPB"/>' $LPINAME.lpi > /dev/null
    if (( $? == 0 ))
    then
        MODE=qemu
    fi
    grep '<TargetController Value="RPIB"/>' $LPINAME.lpi > /dev/null
    if (( $? == 0 ))
    then
        MODE=rpi
    fi
    grep '<TargetController Value="RPI2B"/>' $LPINAME.lpi > /dev/null
    if (( $? == 0 ))
    then
        MODE=rpi2
    fi
    grep '<TargetController Value="RPI3B"/>' $LPINAME.lpi > /dev/null
    if (( $? == 0 ))
    then
        MODE=rpi3
    fi
    grep '<TargetController Value="RPIZERO"/>' $LPINAME.lpi > /dev/null
    if (( $? == 0 ))
    then
        MODE=rpizero
    fi

#   if (( $MODE == "unknown" ))
#   then
#       echo "Cannot determine build target"
#       exit
#   fi

    IMAGE_NAME=$LPINAME-kernel-$MODE.img
    BUILT=0
    if (( $MAKE_KERNELS == 1))
    then
        if [[ -e $IMAGE_NAME ]]
        then
            echo $LPINAME $MODE already built
            BUILT=1
        fi
    fi

    if (( BUILT == 0 ))
    then
        echo $LPINAME $MODE
        rm -rf kernel.img kernel7.img kernel.bin $IMAGE_NAME
        rm -rf lib/
        WD=$(pwd)
        pushd $LAZDIR >& /dev/null
        ./lazbuild $WD/$LPINAME.lpi
        popd >& /dev/null
        mv kernel* $IMAGE_NAME
    fi
}

function buildKernels {
    find subtrees -iname '*.lpi' -print | while read -r LINE
    do
        pushd $(dirname $LINE) >& /dev/null
            SHORTEST_LPI_NAME=$(basename $LINE .lpi)
            grep -il SystemRestartStack *.lpr *.pas >& /dev/null
            USING_STACK=$?
            if [[ $USING_STACK == 0 ]]
            then
                cp $(dirs -l -0)/systemrestartstack.pas $(dirs -l -0)/systemrestartstackwithkeyboard.pas .
                buildLpi $SHORTEST_LPI_NAME
                rm systemrestartstack.pas systemrestartstackwithkeyboard.pas
            fi
        popd >& /dev/null
    done
}

function collectKernels {
    rm -rf samplekernels.inc # samplekernels
    mkdir -p samplekernels
    find subtrees -iname '*kernel*.img' -print | while read -r LINE
    do
        echo "  AddKernel('$(basename $LINE)');" | tee -a samplekernels.inc
        if [[ -e samplekernels/$(basename samplekernels/$LINE) ]]
        then
            echo there $LINE
        else
            cp -a $LINE samplekernels/
        fi
    done
    sort samplekernels.inc > samplekernels2.inc
    mv samplekernels2.inc samplekernels.inc
}

function buildNameMode {
    set -e

    LAZDIR="$HOME/ultibo/core"

    LPINAME=$1
    MODE=$2

    echo $LPINAME $MODE
    rm -rf lib/
    WD=$(pwd)
    pushd $LAZDIR >& /dev/null
    ./lazbuild --build-mode=$MODE $WD/$LPINAME.lpi
    popd >& /dev/null
    mv kernel* $LPINAME-kernel-${MODE,,}.img
}

VERSION=v$(date -u +%Y%m%d)
REPO=ultibo-microbit
ZIPFILE=$REPO-$VERSION.zip

function createRelease {

    mkdir -p release
    rm -rf release/*

    rm -f *kernel*.img
    LPR=ultibomicrobit
    for MODE in RPI RPI2 RPI3
    do
        buildNameMode $LPR $MODE
    done

    set -x
    cp -a *.img release/
    cp -a microbit-hex/ release/
    cp -a $LPR-config.txt $LPR-cmdline.txt release/
    cp -a release/$LPR-config.txt release/config.txt
    echo "$REPO $VERSION" >> release/release-message.md
    echo >> release/release-message.md
    cat release-message.md >> release/release-message.md
    cp -a firmware/boot/bootcode.bin firmware/boot/start.elf firmware/boot/fixup.dat release/

    cd release
    zip -r $ZIPFILE *
    ls -lt $ZIPFILE
    cd ..
}

function testRelease {
    NAME=ultibomicrobit
    rm -rf testrelease
    mkdir testrelease
    cd testrelease
    unzip ../release/$ZIPFILE
    set -x
    sudo rm -rf /boot/microbit-hex
    sudo cp -r microbit-hex/ /boot
    sudo cp $NAME-kernel-rpi3.img /boot
    sudo cp $NAME-cmdline.txt /boot
    sudo cp $NAME-config.txt /boot
    sudo cp config.txt /boot
    sleep 2
    sudo reboot
}

function pushRelease {
    PATH=$HOME/hub-linux-arm-2.3.0-pre10/bin:$PATH
    hub release create -d -p -F release/release-message.md -a release/$ZIPFILE $VERSION
    echo this is a draft release
}

function compressKernels {
    for i in samplekernels/*.img
    do
        bsdiff ref.img $i $i.bsdiff
        ls -lt $i.bsdiff
    done
    ls -lt samplekernels/*.bsdiff
}

function testKernel {
    set -ex

    # on raspbian, build the program and reboot to it

    NAME=ultibomicrobit
    MODE=rpi3

    buildNameMode $NAME $MODE

    sudo rm -rf /boot/microbit-hex
    sudo cp -r microbit-hex /boot

    sudo cp $NAME-kernel-$MODE.img /boot
    sudo cp $NAME-config.txt $NAME-cmdline.txt /boot
    sudo cp /boot/$NAME-config.txt /boot/config.txt
    sleep 2
    sudo reboot
}

SUBTREE=repos/lancaster-university/microbit-samples

function ytClean {
    pushd $SUBTREE >& /dev/null
    yt clean
    popd >& /dev/null
}

function buildExamples {
    SUBTREE=repos/lancaster-university/microbit-samples
    REPODIR=$(pwd)
    mkdir -p microbit-hex
    pushd $SUBTREE >& /dev/null
    if [[ "$1" == "" ]]
    then
        FOLDERS=source/examples/*
        rm -rf $REPODIR/microbit-hex/*
        yt clean
    else
        FOLDERS=source/examples/$1
        yt clean
    fi

    yt target bbc-microbit-classic-gcc
    for EXAMPLE in $FOLDERS
    do
        echo $EXAMPLE
        rm -f source/*.cpp 2> /dev/null
        cp -a $EXAMPLE/*.cpp source/
        rm -f config.json
        if [[ -e $EXAMPLE/config.json ]]
        then
            cp -a $EXAMPLE/config.json .
        fi
        ls source
        yt build
        rm -f config.json
        cp build/bbc-microbit-classic-gcc/source/microbit-samples-combined.hex $REPODIR/microbit-hex/$(basename $EXAMPLE).hex
    done
    popd >& /dev/null
}

case "$*" in
"build examples")
    buildExamples
    ;;
"build blebroadcastbuttonchords")
    buildExamples blebroadcastbuttonchords
    ;;
"build peripheral")
    buildExamples peripheral
    ;;
"compile")
    buildNameMode ultibokernelsampler rpi3
    ;;
"compress kernels")
    compressKernels
    ;;
"create release")
    createRelease
    ;;
"make examples")
    MAKE_KERNELS=1
echo    buildExamples
    ;;
"push release")
    pushRelease
    ;;
"test")
    testKernel
    ;;
"test release")
    testRelease
    ;;
"yt clean")
    ytClean
    ;;
*)
    echo ?
    ;;
esac
