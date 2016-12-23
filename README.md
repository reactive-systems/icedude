# iCEDude<br/>(Version 0.1.1.1)

A programming tool for the iCE40 FPGA evaluation boards produced by
[LATTICE Semiconductor](http://www.latticesemi.com/).

## About this tool

The tool provides an [avrdude](http://www.nongnu.org/avrdude/) like
interface to program and interact with the iCE40 evaluation boards,
e.g., the [iCEblink40-HX1K Evaluation
Kit](http://www.latticesemi.com/iceblink40-hx1k) and the [iCEstick
Evaluation Kit](http://www.latticesemi.com/icestick). The tool is
written in Haskell and uses [libusb](http://www.libusb.org/) as a
backend to communicate with the devices.

The project was inspired by the [iceBurn
project](https://github.com/davidcarne/iceBurn) of David Carne, is
however rewritten completely from scratch. The reason for that was
primarily to increase the number of supported platforms.  Escpecially
since PyUSB is quite buggy and not the best choice when using a
non-Linux environment, e.g., on Apple products. Furthermore, the
rewriting process was useful to understand the interaction with the
different components and to learn about the overall structure.

Nevertheless, we thank David Carne for finding out the different data
values and data structures, which are needed to communicate with the
device.

The tool has been tested on the [iCEblink40-HX1K Evaluation
Kit](http://www.latticesemi.com/iceblink40-hx1k) under a Linux and
under a Mac environment. Feedback for other devices is welcome.

## Installation

iCEDude is written in Haskell and can be compiled using the Glasgow
Haskell Compiler (GHC).

Dependencies:

* [libusb](http://www.libusb.org/) (on mac: `brew install libusb`)

* [GHC](https://www.haskell.org/ghc/) (recommended version: >= 7.6.1, [Haskell2010](https://wiki.haskell.org/Definition))
 
* [usb](https://hackage.haskell.org/package/usb) (recommended version: >= 1.3)

* [mtl](https://hackage.haskell.org/package/mtl) (recommended version: >= 2.2)

* [bytestring](https://hackage.haskell.org/package/bytestring) (recommended version: >= 0.10)

* [directory](https://hackage.haskell.org/package/directory) (recommended version: >= 1.2)

* [transformers](https://hackage.haskell.org/package/transformers) (recommended version: >= 0.4)

* [vector](https://hackage.haskell.org/package/vector) (recommended version: >= 0.10)

Building the tool should be simple using [cabal](https://www.haskell.org/cabal/) 

<code>cabal install</code>

or, if you work in a UNIX environment, simply by using

<code>make</code>

## Usage

```icedude [OPTIONS]...```

### Operations:

| Command                         | Description                                                          |
| ------------------------------- | -------------------------------------------------------------------- |
| ```-l```                        | List all supported devices.                                          |
| ```-d <id>```                   | Select a specific device.                                            |
| ```-U <memtype>:r|w|v:<data>``` | Memory operation specification.                                      |
| ```-e```                        | Perform a chip erase.                                               |
| ```-h```                        | Print this help and exit.                                            |
| ```-v```                        | Quiet output.                                                        |
