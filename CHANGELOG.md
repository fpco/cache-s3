# v0.1.9

* Switch to `unliftio` for temporary file creation. Fix [#23](https://github.com/fpco/cache-s3/issues/23)
* Fix printing of cache creation time [#22](https://github.com/fpco/cache-s3/pull/22)

# v0.1.8

* Addition of `--overwrite` flag that emmits log messages whenever a file is about to be replaced
  upon cache restoration
* Implement concurrent uploading. Addresses the upload part of [#20](https://github.com/fpco/cache-s3/issues/20)

# v0.1.7

* Improved command line documentation: [#15](https://github.com/fpco/cache-s3/issues/15)
* Make `cache-s3` a bit more reselient to errors by not relying on `stack.yaml` format as much: [#17](https://github.com/fpco/cache-s3/issues/17)

# v0.1.6

* Addition ability to store relative paths with `--relative-path` argument: [#11](https://github.com/fp
co/cache-s3/issues/11)

# v0.1.5

* Fixes [#9](https://github.com/fpco/cache-s3/issues/9)

# v0.1.4

* Addition of `--max-size` and `--max-age` arguments.

# v0.1.3

* Fixes caching of files with international names. Proper unicode handling.
* Fixes `Ratio has zero denominator`, when cache size is very small.

# v0.1.2

* Fixes #1, #2 and #3

# v0.1.1

* Initial release
