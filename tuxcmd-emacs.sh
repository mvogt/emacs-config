#!/bin/bash
#
# Wrapper for emacsclient to run from Tux Commander (tuxcmd).  Due to a bug in
# Metacity, editing a file with emacsclient won't raise the Emacs window even
# though it does get the focus.  So, we work around that using wmctrl to raise
# it before calling emacsclient.  We also use wmctrl to see if Emacs is
# running, and if not, we launch it instead of calling emacsclient.
#
# Note: With Compiz enabled, other bugs appear.  Careful testing is required
# to ensure that all launch scenarios operate properly.
#

if wmctrl -x -a emacs.Emacs ; then
    emacsclient --no-wait "$@"
else
    emacs "$@" &
fi
