instantrun
==========

When working in a graphical desktop, there are applications that you probably
launch often, such as a terminal emulator, and your text editor.

Even though these programs usually start up fairly quickly (like half a
second), the wait can be noticeable, and it builds up as you launch many
instances of your applications throughout the course of your day.

The wait is especially frustrating if you just need to perform a short task.
Let's say you want to open a terminal to check the output of `uptime`.  Typing
and running the command itself takes only about a second, so having to wait an
extra half second just for the terminal to start can be maddening. And if you
have a bloated configuration for your shell (such as loading bash completions),
then the wait can be even much longer than half a second.

Working with text editors shares the same problem. If you need to make a small
change to a file, then waiting for Vim to load is painful. During your day, as
you edit many files, this minor inconvenience builds up, and can be bad for
your mental health. If Vim is configured with a lot of plugins then the start
time can even be quite substantial, making the situation much worse. (It is
true that for long editing/coding sessions, one usually uses a single Vim
instance, but I find that in practice I am constantly launching Vim from a
shell in order to open various random files)

Wouldn't it be great if these programs could be started instantly?

`instantrun` is the solution. It is a service that runs in the background, and
pre-starts hidden copies of your applications. When you want to start an
application, all it needs to do is instantly unhide the application's window.

Installation
------------

To compile `instantrun` you need the Haskell [GHC][1] compiler with the `cabal`
toolchain:

    $ cabal configure
    $ cabal build
    $ cabal install

Tutorial
--------

First start the `instantrun` background process (You can do this in `.xsession`
or friends, to have it start automatically):

    $ instantrun --server

Now, if you want to instantly start a program, run it through `instantrun`:

    $ instantrun xterm

The first run of the program will not be instant. But now that `instantrun` has
learned about `xterm`, it will make sure to always have a hidden `xterm` ready
in the background for future runs.

You can also pass command line arguments to your program, such as a file to
edit:

    $ instantrun gvim foo.txt

But wait! Isn't there already supposed to be a `gvim` process already running
in the background? How can command line arguments be sent to an already running
program? In order to accomplish this, `instantrun` uses some magic, and
requires specific configuration for each program. Currently, there is hardcoded
support only for `gvim`, so it is the only program that can receive command
line arguments. (See `src/Config.hs` for the magic details). In the future, an
`instantrun` config file will allow the user to add support for other programs.

In order to save typing, I recommend configuring your shell to create an alias
for your favorite programs:

    $ alias gvim='instantrun gvim'

Now you can easily instantly edit any file:

    $ gvim this-will-open-instantly.txt

If you need to shut down the background server then run:

    $ instantrun --shutdown

To check the status of the background server run:

    $ instantrun --status

For more help:

    $ instantrun --help

Compatibility
-------------

`instantrun` should be compatible with all window managers. I use it with
[xmonad][2] and it works great.

Warning
-------

`instantrun` is incomplete, and currently has lots of known bugs.

- Don't make a typo and try to instantrun a nonexistent program, or the
  background server will hang.

- Don't give commandline arguments to programs other than `gvim` (it is
  currently the only program with commandline arguments support)

[1]: http://www.haskell.org
[2]: http://xmonad.org
