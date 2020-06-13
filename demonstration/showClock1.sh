#!/bin/sh
# showClock1.sh: Show latest difference between clock tix
#   sh showClock1.sh
# 2009-Apr-27 21.00 / TN

PREFIX=tixfiles/StrobeClock_

CURRENT=0

mkdir -p ClockDisplay

while true; do

  NEXT=`expr $CURRENT + 1`
  AFTER=`expr $NEXT + 1`

  while [ ! -f ${PREFIX}${AFTER}.tix ]; do echo $0: Waiting for ${PREFIX}${AFTER}.tix; sleep 1; done

  TWOAFTER=`expr $AFTER + 1`

  if [ -f ${PREFIX}${TWOAFTER}.tix ]; then

    echo $0: Skip over ${PREFIX}${CURRENT}.tix to ${PREFIX}${NEXT}.tix

  else

    echo $0: Mark up ${PREFIX}${CURRENT}.tix to ${PREFIX}${NEXT}.tix

    hpc combine --function=SUB --output=showClock1.tix ${PREFIX}${NEXT}.tix ${PREFIX}${CURRENT}.tix

    hpc markup --highlight-covered showClock1.tix

    mv Main.hs.html ClockDisplay/

  fi

  CURRENT=$NEXT

done
