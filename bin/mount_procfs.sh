#!/bin/sh
if [ $# == 2 ]; then
    mount -o noac,port=22049,mountport=22050,soft,udp,nfsvers=2 \
	localhost:/procfs $1
    mount -o noac,port=22049,mountport=22050,soft,udp,nfsvers=2 \
	localhost:/filefs $2
else
    echo "Usage: $0 <mountpoint>"
fi
