#!/bin/bash
#
# Generate ELisp code on stdout that defines a list variable for all branches
# found by "git branch -a". I massage the list to remove prefixes for remote
# branches and remove entries for manifest pointers from the repo program.
#

set -e

echo "(setq my-vcs-branches '("
git branch -a \
    | cut -c3- \
    | grep -v -e "remotes/m/" \
    | sed -r -e 's|^remotes/[^/]+/||g' \
    | sort -u \
    | sed -e 's/^/"/' -e 's/$/"/'
echo "))"
