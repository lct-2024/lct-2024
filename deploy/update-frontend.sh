#!/bin/bash

set -Eeuo pipefail
set -x

WORK_DIR=${1:-~/projects/lct-2024-latest/frontend/}

cd ${WORK_DIR}

LOG_FILE=/tmp/lct24-frontend-build.log

echo "Pulling: $(date)" >> $LOG_FILE

git pull >> $LOG_FILE 2>&1

echo "Building" >> $LOG_FILE

docker build . -t lct24-frontend >> $LOG_FILE 2>&1

echo "Pulling" >> $LOG_FILE

systemctl --user restart lct-2024 >> $LOG_FILE 2>&1

echo "Done: $(date)" >> $LOG_FILE
