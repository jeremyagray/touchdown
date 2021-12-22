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
LOAD_HELPER= -l test/test-helper.el

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

.PHONY : test
test : elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		$(LOAD_HELPER) \
		-l test/indentation-test.el \
		-l test/highlight-test.el \
		-l test/mode.el \
		-f ert-run-tests-batch-and-exit

elpa : $(ELPA_DIR)

$(ELPA_DIR) : Cask
	$(CASK) install
	touch $(@)
