# ******************************************************************************
#
# touchdown, a major mode for fluentd/td-agent configuration files
#
# Copyright (C) 2016 by Syohei YOSHIDA <syohex@gmail.com>.
# Copyright (C) 2021 Jeremy A GRAY <gray@flyquackswim.com>.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.
#
# ******************************************************************************

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

syntax_files = syntax.el plugins/format/file.el plugins/input/forward.el plugins/input/tail.el plugins/output/file.el plugins/parse/json.el plugins/parse/nginx.el plugins/parse/regexp.el plugins/parse/syslog.el

.PHONY : test
test : elpa
	$(CASK) exec buttercup $(LOADPATH)

.PHONY : lint
lint : elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) --eval "(require 'checkdoc-batch)" -f checkdoc-batch-commandline touchdown.el $(syntax_files)
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) --eval "(require 'package-lint)" -f package-lint-batch-and-exit touchdown.el $(syntax_files)

elpa : $(ELPA_DIR)

$(ELPA_DIR) : Cask
	$(CASK) install
	touch $(@)
