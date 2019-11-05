#!/bin/bash
LOCALVER=`git rev-list HEAD | wc -l`
if [ $LOCALVER \> 1 ] ; then
    VER=`git rev-list origin/master | wc -l`
    VER_DIFF=$(($LOCALVER-$VER))
    if [ $VER_DIFF != 0 ] ; then
        VER="$VER+$VER_DIFF"
    fi
    if git status | grep -q "modified:" ; then
        VER="${VER}M"
    fi
	VER=`echo ${VER} | sed 's/\ //'`
	echo "Revision=${VER}"
    echo "Creating eVersion.txt."
    echo -e $VER > ./Eidomatica/eVersion.txt
	#sed "s/private static final String REVISION.*$/$OUT/g" src/gui/TrackingDialog.java > src/gui/TrackingDialog.java
fi
