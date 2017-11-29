# Artilico
Artistic live-coding in Common Lisp. Why artistic? Oh..

![artilico](/artilico-shot.png)

## Watch your fingers!
This thing inspired by [fluxus](https://en.wikipedia.org/wiki/Fluxus_(programming_environment)). It gently catch your mistakes and let you fly with wings of Lisp. No more stupid crashes. Baa..

## Make it fly

To make it fly you need newest [SBCL](http://www.sbcl.org/platform-table.html) and [quicklisp](https://www.quicklisp.org/beta/) installed. ```freeglut``` library can be installed with Linux package manager, on Windows ```freeglut.dll``` will putted with SBCL binaries.

Then just

```
git clone https://github.com/honix/Artilico.git
cd Artilico
sbcl --load quick-start.lisp

```

## ASDF

You can also load the project using `(ql:quickload :artilico)` and then call `(artilico:gogogo)`

## Controls
It is pretty similar to all you like text editors, except:

```ctrl + Enter``` Evaluate code

## Future
* Better code-editor (optimizations, paredit, autocomplete, colors)
* Artist friendly mechanics
* Sound coding (cl-patterns)
* Use GPU-buffers for fast and glory

## Maybe
* Graphical tablet input
