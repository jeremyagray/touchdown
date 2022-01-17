# touchdown

Major mode for highlighting and editing
[td-agent/fluentd](http://www.fluentd.org/) configuration files.

## Contributing

Check the [issue
tracker](https://github.com/jeremyagray/touchdown/issues) for features
I would like to implement.  Issues involving bugs should include a
description, should modify
[tests/fluentd.conf](https://github.com/jeremyagray/touchdown/blob/main/tests/fluentd.conf)
with the failing configuration text, and should add one or more
failing buttercup tests, that when passed, closes the issue.  All
tests should pass after any change.

## Installation

### Git

Clone this [repo](https://github.com/jeremyagray/touchdown) and load
[`touchdown.el`](https://github.com/jeremyagray/touchdown/blob/main/touchdown.el).
[MELPA support](https://github.com/jeremyagray/touchdown/issues/4) is
hopefully forthcoming.

## Developement Requirements

- [cask](https://github.com/cask/cask)
- [buttercup](https://github.com/jorgenschaefer/emacs-buttercup)
- [undercover](https://github.com/undercover-el/undercover.el)

Installing cask should be sufficient as it will pull in the elisp
dependencies as needed.

## History

Fork of [emacs-fluentd-mode](https://github.com/syohex/emacs-fluentd-mode).

## License

[Gnu GPL version 3](LICENSE.md).

## Authors and Copyright

- Copyright (C) 2016 by Syohei YOSHIDA.
- Copyright (C) 2021 Jeremy A GRAY.
