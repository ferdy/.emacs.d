emacs
=====

My Emacs configuration.

An ongoing, probably never-ending, journey through Emacs Lisp looking for the *right* setup for *my* daily usage.

It requires **Emacs trunk** to work.
I regularly update my sources from here:
```console
$ git clone git://git.savannah.gnu.org/emacs.git
```

I use Emacs on **LinuxBBQ**. This configuration requires some external packages to work, all of them highlighted in the comments when necessary.

The ```esetup``` script helps creating the right environment *before* starting Emacs with this configuration. If it does not work as you expect, *fix it yourself*.

I mainly use Emacs for **Lisp** (Clojure, Common Lisp, Scheme and Elisp) and **LaTeX**, so my setup is planned accordingly.

setup
-----
- clone Emacs trunk and build it
- run ```esetup``` script
- run Emacs

acknowledgements
----------------
This configuration would not have been possible without the work of and the inspiration from these people:
- [Joe Brock](https://github.com/DebianJoe)
- [Mickey Petersen](https://github.com/mickeynp)
- [Sebastian Wiesner](https://github.com/lunaryorn)
- [Artur Malabarba](https://github.com/Bruce-Connor)
- [Sacha Chua](https://github.com/sachac)
- [Bozhidar Batsov](https://github.com/bbatsov)
- [Magnar Sveen](https://github.com/magnars)

warranty
----
This configuration is provided with *absolutely no warranty*, *no guarantee* and *no support*.
