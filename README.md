# Artilico
Artistic live-coding in Common Lisp. Why artistic? Oh..

![artilico](/artilico-shot.png)

## Watch your fingers!
This thing inspired by [fluxus](https://en.wikipedia.org/wiki/Fluxus_(programming_environment)). It gently catches your mistakes and let you fly with wings of Lisp. No more stupid crashes. Baa..

## Make it fly

To make it fly you need newest [SBCL](http://www.sbcl.org/platform-table.html) and [quicklisp](https://www.quicklisp.org/beta/) installed. ```freeglut``` library can be installed with Linux package manager, on Windows ```freeglut.dll``` will putted with SBCL binaries.

Then just

```
cd ~/quicklisp/local-projects
git clone https://github.com/honix/Artilico.git
sbcl --load Artilico/quick-start.lisp

```

## ASDF

You can also load the project using `(ql:quickload :artilico)` and then call `(artilico:gogogo)`

## Controls
It is pretty similar to all you like text editors, except:

```ctrl + Enter``` Evaluate code

## TODO
-- Text select
-- Copy/Paste
-- Top-level forms in separate units / 2D pannable surface