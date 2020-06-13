#!/bin/sh
# strobeHpcDiff1.sh: Difference between tix strobe files 1
#   sh strobeHpcDiff1.sh source-directory strobe-tix-directory strobe-tix-file-suffix strobe-from strobe-to
# 2008-Sep-14 11.57 / TN
# 2009-May-08 17.43 / TN: Replace "==" by "=" in tests

COUNT=0
for F in *.html; do
  if [ -e "$F" ]; then
    COUNT=`expr $COUNT + 1`
    echo 1>&2 $0: Found .html file \"$F\"
  fi
done

if [ "$COUNT" != 0 ]; then
  echo 1>&2 $0: Please clean out .html files
  exit 1
fi

if [ "$#" = "0" ]; then
  echo 1>&2 $0: Please specify source-directory parameter
  exit 1
fi
SOURCE_DIRECTORY="$1"
shift

if [ "$#" = "0" ]; then
  echo 1>&2 $0: Please specify strobe-tix-directory parameter
  exit 1
fi
STROBE_TIX_DIRECTORY="$1"
shift

if [ "$#" = "0" ]; then
  echo 1>&2 $0: Please specify strobe-tix-file-suffix parameter
  exit 1
fi
STROBE_TIX_FILE_SUFFIX="$1"
shift

if [ "$#" = "0" ]; then
  echo 1>&2 $0: Please specify strobe-from parameter
  exit 1
fi
STROBE_FROM="$1"
shift

if [ "$#" = "0" ]; then
  echo 1>&2 $0: Please specify strobe-to parameter
  exit 1
fi
STROBE_TO="$1"
shift

if [ "$#" != 0 ]; then
  echo 1>&2 $0: There are $# superfluous parameters: "$@"
  exit 1
fi

STROBE_TIX_SUFFIX=${STROBE_TIX_DIRECTORY}/${STROBE_TIX_FILE_SUFFIX}_
STROBE_TIX_OUTPUT=${STROBE_TIX_FILE_SUFFIX}_${STROBE_TO}_SUB_${STROBE_FROM}
STROBE_TIX_OUTPUT_FILE=${STROBE_TIX_OUTPUT}.tix

hpc combine --function=SUB --output=${STROBE_TIX_OUTPUT_FILE} ${STROBE_TIX_SUFFIX}${STROBE_TO}.tix ${STROBE_TIX_SUFFIX}${STROBE_FROM}.tix
RC="$?"
if [ "$RC" != 0 ]; then
  echo 1>&2 $0: hpc combine: Unexpected return code $RC
  exit 1
fi

hpc markup --highlight-covered --srcdir=${SOURCE_DIRECTORY} ${STROBE_TIX_OUTPUT_FILE}
RC="$?"
if [ "$RC" != 0 ]; then
  echo 1>&2 $0: hpc combine: Unexpected return code $RC
  exit 1
fi

WORKING_DIR=strobeHpc_${STROBE_TIX_OUTPUT}_`date +%Y%m%d_%H%M%S`

mkdir ${WORKING_DIR}
RC="$?"
if [ "$RC" != 0 ]; then
  echo 1>&2 $0: mkdir ${WORKING_DIR}: Unexpected return code $RC
  exit 1
fi

mv `grep -l '"istickedoff"' *.html` ${WORKING_DIR}
RC="$?"
if [ "$RC" != 0 ]; then
  echo 1>&2 $0: mv `grep -l '"istickedoff"' ${WORKING_DIR}/*.html` .: Unexpected return code $RC
  exit 1
fi

tar cvzf ${WORKING_DIR}.tar.gz ${WORKING_DIR}

rm *.html

# Exit OK:

exit 0
