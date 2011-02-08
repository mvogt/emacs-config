#!/bin/bash
#
# Copyright 2011  Mark Vogt
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

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
