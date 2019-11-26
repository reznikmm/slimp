# slimp
[![Build Status](https://travis-ci.com/reznikmm/slimp.svg?branch=master)](https://travis-ci.com/reznikmm/slimp)

> Implementation of SlimProto TCP protocol

This project implements
[SlimProt](http://wiki.slimdevices.com/index.php/SlimProtoTCPProtocol),
audio streaming protocol used in the Logitech Media Server and
corresponding players like
[Logitech's Squeezeboxes](http://wiki.slimdevices.com/index.php/Squeezebox_Family_Overview)
or [SqueezeEsp32](https://github.com/bgiraut/SqueezeEsp32).

## Install

Run
```
make all install PREFIX=/path/to/install
```

### Dependencies
It depends on [Matreshka](https://forge.ada-ru.org/matreshka) library.

## Usage

To start server just run `slim-run` executable.

To use as a library, add `with "slimp";` to your project file.

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to join!
[Open an issue](https://github.com/reznikmm/anagram/issues/new) or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik
