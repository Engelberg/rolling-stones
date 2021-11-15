# Rolling Stones Change Log

## 1.0.2

* Updated to use version 2.3.6 of SAT4J
* Fixed memory leak that occurred when no timeout is set
* Added note to README that when timeout is set (in milliseconds), solver can't be garbage collected until timeout is reached.
* Added `*timeout-on-conflicts*` dynamic variable to control whether timeout integer is interpreted as number of conflicts instead of milliseconds.

## 1.0.1

### Bugfixes

* Fixed bug in `and?`

## 1.0.0 - Initial Release

