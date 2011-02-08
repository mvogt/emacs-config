My Emacs Configuration
======================

Here's my personal Emacs configuration in case it's useful to anyone.  This readme only describes the more significant features.


grep-compile.el
---------------

I wrote the function my-cur-word-or-region to return a string containing the word that surrounds the point.  If the current region is active, it returns that instead.  I call this from various other functions when I want a default value based on the user's context.

The original function from which I call it is my-recursive-grep.  That basically executes `find | xargs grep foo` and places the result in a grep-mode buffer.  Its extra features are:

* Guesses the search term from context.
* The results accumulate in the results buffer live.
* Prompts for the starting directory, defaulting to the cwd.
* The full search command line can be edited before executing.
* Previous search commands and starting directories are saved in the history.
* Always skips the .git sub-tree.
* Prefix args modify the command line for variations I find helpful:
  * no prefix: search all files
  * single C-u prefix: search files matching a pattern
  * double C-u prefix: the search pattern is the file name itself (i.e. don't run grep)


kill-yank.el
------------

I've changed the prefix behavior of kill and yank (a.k.a. cut/copy/paste) with the custom functions my-kill-region, my-kill-ring-save, and my-yank.  A numeric prefix 0 through 9 operates on an Emacs register of the same name.  For example, a prefix argument of 1 to my-kill-region does an ordinary kill-region, but it also stores the killed text in register 1.  Similarly, a prefix argument of 1 to my-yank will insert the text from register 1 instead of from the kill ring.  Without a prefix, my-yank just yanks from the kill ring.  Both my-kill-region and my-kill-ring-save copy to register 0 when no prefix is specified.

(Emacs supports more than 10 registers, but I find 10 is plenty for me.  Also, I don't want to hassle with somehow converting numeric prefix args into alphabetic registers.)

This feature is useful when I need to interleave pasting a constant string of text with frequent kill-region operations.


my-shell.el
-----------

I've added some features to shell-command with the wrapper function my-interactive-shell-command.  If you exit the minibuffer with C-Enter instead of Enter, the command runs in the background without waiting for the result, and the result is discarded.  If you exit the minibuffer with M-Enter, the command is saved and not executed.  It will be the default command on the next call to the same function.  Also, a prefix arg causes the current region or word surrounding the point to be appended to the minibuffer.

I also use this enhancement in dired mode when executing a command on one or more selected files.


search-replace.el
-----------------

I wrote replace-special-chars to convert common Unicode punctuation into ASCII.  The vast majority of my text files are 7-bit ASCII, and I don't want to pollute them with Unicode when pasting text from the web or other documents.

The functions query-replace-multibuf and query-replace-regexp-multibuf perform a query-replace across all buffers.  This is useful for a global rename operation.


my-completion.el
----------------

The function my-toggle-completions-window is a quick hack so I can live with the default minibuffer completion mechanism until I get comfortable with something like [ido] [1] or [icicles] [2].  It toggles between the minibuffer and the completions window, and I map it to S-Tab.

[1]: http://www.emacswiki.org/emacs/InteractivelyDoThings "ido"
[2]: http://www.emacswiki.org/emacs/Icicles "icicles"


buffer-select.el
----------------

The bs-show feature built into Emacs (bs.el) calls bs-select when you tell it to display one or more buffers.  I redefined bs-select to call the custom function my-tile-windows when more than one buffer is selected.  It creates a tiled layout of windows where the minimum width is half of split-width-threshold, and the heights are as equal as possible.  Here are some example layouts in ASCII art:

* Frame wide enough to support only 1 window:
<pre>
    2 bufs     3 bufs     4 bufs
    +-----+    +-----+    +-----+
    |     |    |     |    |     |
    |     |    |     |    +-----+
    |     |    +-----+    |     |
    +-----+    |     |    +-----+
    |     |    +-----+    |     |
    |     |    |     |    +-----+
    |     |    |     |    |     |
    +-----+    +-----+    +-----+
</pre>

* Frame wide enough to support 2 windows:
<pre>
      2 buffers        3 buffers        4 buffers
    +-----+-----+    +-----+-----+    +-----+-----+
    |     |     |    |     |     |    |     |     |
    |     |     |    |     |     |    |     |     |
    |     |     |    |     +-----+    +-----+-----+
    |     |     |    |     |     |    |     |     |
    |     |     |    |     |     |    |     |     |
    +-----+-----+    +-----+-----+    +-----+-----+
</pre>

* Frame wide enough to support 3 windows:
<pre>
         2 buffers              3 buffers              4 buffers
    +--------+--------+    +-----+-----+-----+    +-----+-----+-----+
    |        |        |    |     |     |     |    |     |     |     |
    |        |        |    |     |     |     |    |     |     |     |
    |        |        |    |     |     |     |    |     |     +-----+
    |        |        |    |     |     |     |    |     |     |     |
    |        |        |    |     |     |     |    |     |     |     |
    +--------+--------+    +-----+-----+-----+    +-----+-----+-----+

         5 buffers              6 buffers              7 buffers
    +-----+-----+-----+    +-----+-----+-----+    +-----+-----+-----+
    |     |     |     |    |     |     |     |    |     |     |     |
    |     |     |     |    |     |     |     |    |     |     +-----+
    |     +-----+-----+    +-----+-----+-----+    +-----+-----|     |
    |     |     |     |    |     |     |     |    |     |     +-----+
    |     |     |     |    |     |     |     |    |     |     |     |
    +-----+-----+-----+    +-----+-----+-----+    +-----+-----+-----+
</pre>
