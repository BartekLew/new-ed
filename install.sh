#!/bin/bash -x

if [[ -z "$1" ]]; then
    BIN="/usr/local/bin/"
else 
    BIN="$1"
fi

if [[ -z "$2" ]]; then
    LIB=`echo 'my @x = grep { m#^/usr/lib/x.*/\d+\.\d+$# } @INC; print $x[0]' | perl`
else 
    LIB="$2"
fi

cp perepl.pl $BIN
cp -r Perepl $LIB
