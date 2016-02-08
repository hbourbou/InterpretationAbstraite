#!/bin/sh
NBITER=100000000
myfile="filename"

k=1
while [ $k -le $NBITER ]
do
  echo "x = x+y" >> $myfile
  k=$((k+1))
done

exit 0
