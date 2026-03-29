# Changelog

## Version 0.6.4.9000

- Updates docs and tests for [ggplot2](https://ggplot2.tidyverse.org)
  release (`3.4.0`).

## Version 0.6.4

CRAN release: 2022-10-13

- Gets rid of `NOTE` in CRAN checks.

## Version 0.6.3

CRAN release: 2021-09-09

- Introducing `orientation` argument to control the direction (either
  `x` or `y`) of the layer and better compatible with
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).
  ([@xiangpin](https://github.com/xiangpin),
  [\#104](https://github.com/const-ae/ggsignif/issues/104))

## Version 0.6.2

CRAN release: 2021-06-14

- Updates visual regression tests for `vdiffr 1.0.0` release.

- Removes `NOTE` about `lazyData` from CRAN’s daily checks.

## Version 0.6.1

CRAN release: 2021-02-23

- [@IndrajeetPatil](https://github.com/IndrajeetPatil) is now a
  `ggsignif` author in recognition of his significant and sustained
  contributions.

- Add `extend_line` parameter to make the brackets shorter or wider
  ([\#70](https://github.com/const-ae/ggsignif/issues/70)). Thanks to
  [@romanhaa](https://github.com/romanhaa) for making the PR.

- Adds a new package website: <https://const-ae.github.io/ggsignif/>.

- Minimum `ggplot2` version needed is bumped to `3.3.3`.

- Minor internal refactoring

- Adds snapshot and visual regression testing infrastructure.

## Version 0.6.0

CRAN release: 2019-08-08

- Support plotmath expression and add new parameter `parse` to function.
  Fixes issue [\#64](https://github.com/const-ae/ggsignif/issues/64).
  Thanks to [@IndrajeetPatil](https://github.com/IndrajeetPatil) for the
  idea.

## Version 0.5.0

CRAN release: 2019-02-20

- Fix typos in README.md (thanks to
  [@SMargell](https://github.com/SMargell))

- `map_signif_level` can now take a user-supplied function to format the
  p-value (PR [\#52](https://github.com/const-ae/ggsignif/issues/52),
  thanks to [@ilia-kats](https://github.com/ilia-kats))

## Version 0.4.0

CRAN release: 2017-08-03

- Fix bug that stopped textsize from working

- Add `manual=TRUE` mode, which allows the parameters to be given as a
  data.frame

## Version 0.3.0

CRAN release: 2017-06-25

- Simplify setting brackets at custom locations with xmin, xmax and
  y_position

- Extend documentation

- Bug fixes

## Version 0.2.0

CRAN release: 2017-04-26

- Fixed bug, when
  [`alpha()`](https://scales.r-lib.org/reference/alpha.html) from
  another package was loaded (issue
  [\#2](https://github.com/const-ae/ggsignif/issues/2))

- Added additional parameters to customize bracket linetype, textsize,
  size (issue [\#5](https://github.com/const-ae/ggsignif/issues/5))

- Fixed bug when annotation was identical for different brackets (issue
  [\#6](https://github.com/const-ae/ggsignif/issues/6))

- Fixed bug when multiple comparisons referenced the same block (issue
  [\#8](https://github.com/const-ae/ggsignif/issues/8))

## Version 0.1.0

CRAN release: 2017-04-05

- Initial release on CRAN: <https://CRAN.R-project.org/package=ggsignif>
