emacs
=====

My Emacs configuration.

It requires **Emacs trunk** to work. I regularly update my sources from here:
```console
$ git clone git://git.savannah.gnu.org/emacs.git
```

I use Emacs on **LinuxBBQ**. This configuration requires some external packages, all of which are highlighted in the comments when necessary. The packages can be easily installed using ```apt-get```.

The ```esetup``` script helps to create the right environment *before* starting Emacs with this configuration for the first time. The script only works with Debian-based systems. If it does not work as you expect, *fix it yourself*.

I mainly use Emacs for **Lisp** (Clojure, Common Lisp, Scheme and Elisp) and **LaTeX**, so my setup is planned accordingly.

Before getting started
----------------------
Some things you should know before getting started with this configuration:
- excluding ```init.el```, all the details of this configuration are in ```.emacs.d/lisp```
- there are *over 90 packages* already configured and ready to be used
- be sure to check *all* the keybindings before doing something crazy

Setup
-----
- clone Emacs trunk and build it
- run ```esetup``` script
- run Emacs

Acknowledgements
----------------
This configuration would not have been possible without the work of and the inspiration from these people:
- [Joe Brock](https://github.com/DebianJoe)
- [Mickey Petersen](https://github.com/mickeynp)
- [Sebastian Wiesner](https://github.com/lunaryorn)
- [Artur Malabarba](https://github.com/Bruce-Connor)
- [Sacha Chua](https://github.com/sachac)
- [Bozhidar Batsov](https://github.com/bbatsov)
- [Magnar Sveen](https://github.com/magnars)

Warranty
--------
This configuration is provided with *absolutely no warranty*, *no guarantee* and *no support*.
