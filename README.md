# touchdown

Major mode for highlighting and editing
[td-agent/fluentd](http://www.fluentd.org/) configuration files.

## Roadmap

- [-] rename lisp variables and functions to reflect the names in the
  fluentd configuration file syntax
- [] tab completion
  - [] directives
  - [] core plugin parameters
  - [] tags
  - [] labels
  - [] parameter values
    - [] tags
	- [] paths
- [] find a local coverage report generator (like coverage,
  simplecov-html, or istanbul)
- [] add explicit regular expressions for core plugin parameters
- [] run `td-agent --dry-run --config` on file
- [x] add explicit regular expressions for second-level directives
- [x] add explicit regular expressions for top-level directives
- [x] add undercover.el coverage
- [x] fix bug that treats commented <...> as a directive
- [x] port tests to buttercup

## Contributing

Check the roadmap for features I would like to implement.  Issues
involving bugs should include a description, should modify
`tests/fluentd.conf` with the failing configuration text, and a
failing buttercup test, that when passed, closes the issue.

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
