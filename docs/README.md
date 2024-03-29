# Emperor

_A statically typed, automatically-threaded, imperative language with purity and functional constructs._

## Description

This is the compiler for the [`emperor`](https://emperor-lang.github.io/emperor/) language which is designed to be run from the command-line.
The CLI of this project is specified in [`emperor.json`][emperor.json] which conforms to [`argspec`][argspec].

The language itself is intended to have similarities to functional languages, including an expressive type-system.
However, it is intended to allow the user to explicitly manipulate state with a notion of both _pure_ and _impure_ functions.
It is possible to use local state within functions however global state and other side-effects may only be used from _impure_ functions.

Dynamic scope is used for variables but static scoping is used for functions.

## Usage

The emperor compiler may be called as follows.

```bash
emperor -i ./program.e -o ./program
```

Single programs may be translated in to C and compiled separately along with other C files as follows.

```bash
emperor -c -O3 -i ./program.e -o ./program.e.c
gcc $(emperor-setup --cflags) ... ./program.e.c ... $(emperor-setup --libs)
```

### Installation &amp; Dependencies

For complete installation from the source, this project requires the following:

- [`arggen_haskell`][arggen] argument parser generator from [`argspec`][argspec] json files
- [`ghc`][ghc] Glasgow Haskell Compiler
- [`alex`][alex] Haskell lexer generator
- [`happy`][happy] Haskell parser generator
- [`patch`][patch] the applicator of [`diff`][diff] patches.
- [`make`][make] the GNU tool for generating executables

The following are optional but may be useful development.

- [`mangen`][mangen] an [`argspec`][argspec] to man-page generator
- [`haddock`][haddock] the haskell documentation generator
- Haskell platform documentation

Once all of these have been installed, the emperor compiler may be installed from the source.
This can be done as follows.

```bash
git clone https://github.com/emperor-lang/emperor.git
cd ./emperor/
make install
```

This will install `emperor` at `/usr/bin/emperor`, along its `man` page.
For testing purposes, running just `make` will produce a single binary.

## Contributing

If you would like to contribute to the project, please take a look at the short guidelines [here][contributing].

## Code of Conduct

When interacting with this project, please adhere to the code of conduct found [here][code-of-conduct].

## License

This project is distributed under the [GPL-v3.0][license] license.

## Author

This project is primarily maintained by Edward Jones.

[emperor.json]: https://github.com/emperor-lang/emperor/blob/master/emperor.json
[ghc]: https://www.haskell.org/ghc/
[alex]: https://www.haskell.org/alex/
[happy]: https://www.haskell.org/happy/
[patch]: https://linux.die.net/man/1/patch
[diff]: https://linux.die.net/man/1/diff
[make]: https://gnu.org/software/make
[argspec]: https://github.com/argspec/argspec
[arggen]: https://github.com/argspec/arggen
[mangen]: https://github.com/argspec/mangen
[haddock]: https://www.haskell.org/haddock/
[contributing]: https://github.com/emperor-lang/emperor/blob/master/.github/CONTRIBUTING.md
[code-of-conduct]: https://github.com/emperor-lang/emperor/blob/master/.github/CODE_OF_CONDUCT.md
[license]: https://github.com/emperor-lang/emperor/blob/master/LICENSE
