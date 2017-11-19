#!/bin/sh

set +euf

if [ $# -eq 0 ]; then
    cat <<EOF
Please specify a title for the migration.

Usage: $0 <title>
EOF
    exit -1;
fi

CURRENT_DIR=$(dirname $(readlink -f $0))
MIGRATION_TITLE="$1"
MIGRATION_FILE="$CURRENT_DIR/migrations/$(date +%s)-$MIGRATION_TITLE.sql"

(cat <<EOF
begin;

commit;
EOF
) > "$MIGRATION_FILE";

echo "Created new migration $MIGRATION_TITLE";
