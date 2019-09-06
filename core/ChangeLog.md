### next

- Replace `regex-posix` dependency with `regex-tdfa` for better Windows and GHCJS support

### 0.8.2.0

- Add `Semigroup` instances
- Avoid orphan-instance clashing by using `base-orphans`'s orphan `Functor` instances `base` < 4.7
- Make `-Wall` clean

#### 0.8.1.1

- Fix a build warning that happens to be a fatal error with GHC 7.4 when using `ansi-wl-pprint` < 0.6.6

### 0.8.1.0

- Add `Applicative` instances
- Add support for `time-1.5.0`
