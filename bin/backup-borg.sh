#!/bin/sh

WINHOME=/mnt/c/Users/kristoffer/
REPOSITORY="${WINHOME}/Google Drive/docs.attic/"

# Backup all of /home and /var/www except a few
# excluded directories
borg create --stats                                \
    "${REPOSITORY}"::`hostname`-`date +%Y-%m-%d`   \
    ${WINHOME}/Documents                           \
    --exclude ${WINHOME}/Documents/ETH/NuM\ 2010   \
    --exclude '*.pyc'

# Use the `prune` subcommand to maintain 7 daily, 4 weekly
# and 6 monthly archives.
borg prune -v "${REPOSITORY}" --keep-daily=7 --keep-weekly=4 --keep-monthly=6
