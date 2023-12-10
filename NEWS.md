# dssrip2 (development version)

# dssrip2 0.8-0

- Added Linux support (tested with Ubuntu 22.04).
- `dss_require()` is now silent (as it was always supposed to be).
- Fixed bug when writing `tibble` objects to DSS.

# dssrip2 0.7-1

- `dss_connect()` now issues a warning if the JVM has limited memory resources.

# dssrip2 0.7-0

- Revamped apporach to file handling. DSS files are now handled
  internally byt the package and users no longer need to explicitly
  open or close DSS files.
- New funciton `dss_convert()` to convert between DSS-6 and DSS-7 files.
