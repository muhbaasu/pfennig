#!/bin/sh

set +euf

USER="${PQ_USER:-postgres}"
DATABASE="${PQ_DATABASE:-pfennig}"

CURRENT_DIR=$(dirname $(readlink -f $0))

find "${CURRENT_DIR}/migrations/" -maxdepth 1 -type f \
        | sort \
        | xargs -I{} psql -U "$USER" --file={} "$DATABASE"
