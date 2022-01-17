# touchdown

Major mode for highlighting and editing
[td-agent/fluentd](http://www.fluentd.org/) configuration files.

## Roadmap

- [-] rename all symbols according to visibility (`touchdown--` or `touchdown-`)
- [-] completion
  - [] add syntax information for plugins and parameters
  - [] use parameter information for completing parameter lines
  - [x] use line identification functions to walk syntax data
- [] organize `touchdown.el` according to symbol's pupose
- [] remove `interactive` from all hidden functions
- [] line identification and walking
  - [] add line identification predicates
- [] implement full test coverage
- [x] load large sections from separate files (i.e. syntax data)
- [x] rename all syntax types systematically
- [x] implement debug setting in place of per function arguments
- [x] add explicit regular expressions for second-level directives
- [x] add explicit regular expressions for top-level directives
- [x] add undercover.el coverage
- [x] fix bug that treats commented <...> as a directive
- [x] fix comment highlighting in parameter values
- [x] hack simplecov to generate a coverage report
- [x] port tests to buttercup
- [x] rename lisp symbols to match the fluentd configuration file syntax
- [x] run `td-agent --dry-run --config` on file

## Contributing

Check the roadmap for features I would like to implement.  Issues
involving bugs should include a description, should modify
`tests/fluentd.conf` with the failing configuration text, and should
add one or more failing buttercup tests, that when passed, closes the
issue.  All tests should pass after any change.

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
