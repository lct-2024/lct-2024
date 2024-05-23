#!/bin/bash

for service in passport; do
    docker build --target "${service}" -t "lct2024-${service}" .
done
