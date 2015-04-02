# Changelog

## 0.3.1

* Correctly demonitor the worker process in `noesis_task:await/2`

## 0.3

[Documentation](http://noesis.nifoc.pw/0.3/)

* Added `noesis_geometry` module with some (more or less) geometry related functions
* Added `noesis_math` module
* Added `noesis_polyline` module, which implements encoding and decoding [polylines](https://developers.google.com/maps/documentation/utilities/polylinealgorithm)
* Added `noesis_task` module, which implements the async/await pattern
* Added `noesis_xml` module, which includes some very basic functions for working with XML

## 0.2.1

[Documentation](http://noesis.nifoc.pw/0.2.1/)

* `noesis_proplists:extract/{2,3,4}` now ignores duplicate entries in the list of keys that should be extracted
* `noesis_datetime:rfc1123/1` now prefixes one, two or three digit years with zeros

## 0.2

[Documentation](http://noesis.nifoc.pw/0.2/)

* Switch from [Rebar](https://github.com/rebar/rebar) to [erlang.mk](https://github.com/ninenines/erlang.mk)
* `noesis_lists`: Add `pfilter/{2,3}`, `pmap/{2,3}`, `split/2`

## 0.1

[Documentation](http://noesis.nifoc.pw/0.1/)

* Initial release
