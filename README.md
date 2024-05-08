# MUSIC V

[![Actions Status](https://github.com/vlazzarini/MUSICV/workflows/ci/badge.svg)](https://github.com/vlazzarini/MUSICV/actions)

This is Max Mathew's MUSIC V synthesis program, prepared by
Bill Schottstaed for gfortran, with further
additions and fixes by Victor Lazzarini.

## Building

To build MUSIC V you will need to install a Fortran compiller such
as [GFortran](http://gcc.gnu.org/wiki/GFortran), has been tested and the sources
are kept up to date with it. You will also need CMake and a C
compiler installation.

With these installed, build MUSIC V:

```sh
cmake -B build
cmake --build build
```

## Installing

Installation is controlled by the CMake option
`CMAKE_INSTALL_PREFIX`.

The command

```sh
cmake --install build
```

will install the software to `${CMAKE_INSTALL_PREFIX}/bin`. Depending
on the local admin permissions might be need to perform installation.


## Running

MUSIC V is based on a 3-pass set of commands (which take no arguments):

1. pass1  takes a score file named 'score' and produce 'pass1.data'
2. pass2  takes a 'pass1.data' file and produces 'pass2.data'
3. pass3  takes a 'pass2.data' file and produces 'snd.raw'

'snd.raw' is a mono, 44.1KHz, 32-bit float with your system's
endianess. A utility, `raw2wav` is provided to convert this to a
RIFF-Wave format file.

The `music5` command is also included. This is a driver program that
calls the three passes and produces a RIFF Wave format
soundfile.

```
music5 <scorefile> <soundfile>
```

Since "music5" calls the other commands, it is
important that these are all found in the same directory.


Known Issues
------

- The FLT unit generator is missing.
- PLF routines cannot always be directly added to pass1.f because the read1
  routine has been disposed of. They may need to be integrated in the main
  program.
