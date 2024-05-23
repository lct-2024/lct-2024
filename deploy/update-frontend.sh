#!/bin/bash

set -Eeuo pipefail
set -x

WORK_DIR=${1:-~/projects/lct-2024-latest/frontend/}

cd ${WORK_DIR}

git pull

docker build . -t lct24-frontend > /tmp/lct24-frontend-build.log 2>&1

systemctl --user restart lct-2024
