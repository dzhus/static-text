# Changelog

## [Unreleased]

### Changed

- Package renamed to `static-text` as per
  <https://github.com/dzhus/static-text/issues/2>. Old names were
  changed as follows:

    | before       | after             |
    |--------------|-------------------|
    | `Data.Sext`  | `Data.StaticText` |
    | `Sext n a`   | `Static a n`      |
    | `Sextable a` | `IsStaticText a`  |
    | `$(sext ..)` | `$(st ..)`        |

## [0.1.3.1] - 2017-10-29

### Added

- GHC 8.2.x support

## [0.1.3] - 2017-03-26

### Added

- `ShortByteString` support

### Fixed

- A bug in `createLeft` which failed to actually pad/truncate strings
  (reported by Altai-man <https://github.com/dzhus/static-text/issues/4>)

## [0.1.2] - 2017-01-18

### Added

- `Vector` support, `Eq` and `Ord` instances (contributed by Dylan
  Simon <https://github.com/dzhus/static-text/pull/3>)

## [0.1.1] - 2016-19-12

### Added

- GHC 8.0.x support

## [0.1.0.2] - 2015-12-06

### Added

- GHC 7.10.x support

## [0.1.0.0] - 2014-08-10

[0.1.3.1]: https://github.com/dzhus/static-text/compare/0.1.3...0.1.3.1
[0.1.3]:   https://github.com/dzhus/static-text/compare/0.1.2...0.1.3
[0.1.2]:   https://github.com/dzhus/static-text/compare/0.1.1...0.1.2
[0.1.1]:   https://github.com/dzhus/static-text/compare/0.1.0.2...0.1.1
[0.1.0.2]: https://github.com/dzhus/static-text/compare/0.1.0.0...0.1.0.2
[0.1.0.0]: https://github.com/dzhus/static-text/tree/0.1.0.0
