# Tetris

Haskell Tetris game implementation based on [gloss](https://hackage.haskell.org/package/gloss) library.


## Installation

1. Download and install [stack](https://github.com/commercialhaskell/stack).
2. Download repository with `git clone git@github.com:SkyA1ex/tetris.git`
3. Setup ghc with `stack setup`. It will download the compiler if necessary in an isolated stack location (default `~/.stack`) that won't interfere with any system-level installations.
4. Build project with `stack build`. You should call it every time you make changes.
5. Launch project with `stack exec tetris`. stack know where to find executable (`./stack-work`).

## Project structure

* `src/` directory contains source code files (each file is module).
* `app/` directory must contains files to be executable. In our case there is only `main :: IO ()`
* `test/` directory doesn't used.
* `tetris.cabal` – package configuration file (each project can have multiple packages). This file specifies which packages are dependencies.
* `stack.yaml` – project configuration file. This file specifies which packages are available to be used and which packages to include (`packages` property). `extra-deps` property specifies additional dependencies that are not in [LTS](https://www.stackage.org/lts-3.17).

## Git

Project is developed using simplified [A successful Git branching model](http://nvie.com/posts/a-successful-git-branching-model/). There are two main branches: `master` and `dev`. Both `dev` and `feature-[name]` branches (which are merges) used for development. `master` branch always have last release version of the game.

## Contributors

* [Ambartsumyan Vladislav](https://github.com/vladambartsumyan)
* [Anton Ermolinsky](https://github.com/OQJAV)
* [Alexander Tkachenko](skyalexx@gmail.com)


## License

    The MIT License

    Copyright (c) 2015-2016 Tetris project contributors

    https://github.com/skya1ex/tetris/graphs/contributors

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
