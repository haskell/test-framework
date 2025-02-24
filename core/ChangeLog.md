#### 0.8.2.1

- Support `random-1.3`
- Drop support for GHC 7
- Drop support for dependency versions that predate Stackage LTS 9.21
- Tested building with GHC 8.0 - 9.12.1

### 0.8.2.0

- Add `Semigroup` instances
- Avoid orphan-instance clashing by using `base-orphans`'s orphan `Functor` instances `base` < 4.7
- Make `-Wall` clean

#### 0.8.1.1

- Fix a build warning that happens to be a fatal error with GHC 7.4 when using `ansi-wl-pprint` < 0.6.6

### 0.8.1.0

- Add `Applicative` instances
- Add support for `time-1.5.0`
