# deash
A shell with ALGOL-Style syntax for scripting and a name of which I do not remember the meaning of.

## Building
deash requires the freepascal compiler (fpc) >= 3.2.2 and windres for compilation.

To build it simply run the `build.sh` script from this repo, if you wish to have debug messages simply supply the script with the `debug` parameter.

## Documentation
See the `docs` directory for various documents such as the deash specification (`deash_spec.sad`).
Documents in the `sad` format can be converted to html using sadhtml. This tool can be found in
the [sad repository](https://github.com/FelixEcker/sad).
