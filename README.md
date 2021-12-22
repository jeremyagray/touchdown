# touchdown

Major mode for highlighting and editing
[td-agent/fluentd](http://www.fluentd.org/) configuration files.

## Roadmap

- [] fix bug that treats commented <...> as a directive
- [] move testing to buttercup
- [] find a local coverage report generator
- [] rename lisp variables and functions to reflect the names in the
     fluentd configuration file syntax
- [] add explicit regular expressions for top-level directives
- [] add explicit regular expressions for second-level directives
- [] add explicit regular expressions for core plugin parameters
- [] run dry-run on file

## Installation

### Git

Clone this [repo](https://github.com/jeremyagray/touchdown) and load
[`touchdown.el`](touchdown.el).

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
