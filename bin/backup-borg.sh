#!/bin/sh
REPOSITORY='/cygdrive/c/Users/kristoffer/Google Drive/docs.attic/'

# Backup all of /home and /var/www except a few
# excluded directories
attic create --stats                                \
    "${REPOSITORY}"::agamemnon-`date +%Y-%m-%d`      \
    /cygdrive/g/Documents                           \
    --exclude /cygdrive/g/Documents/ETH/NuM\ 2010   \
    --exclude '*.pyc'

# Use the `prune` subcommand to maintain 7 daily, 4 weekly
# and 6 monthly archives.
attic prune -v "${REPOSITORY}" --keep-daily=7 --keep-weekly=4 --keep-monthly=6
