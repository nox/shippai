# Copyright (c) 2013, Anthony Ramine <n.oxyde@gmail.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

ERL = erl
DIALYZER = dialyzer
CT_RUN = ct_run

PLT = .shippai_plt
COVER_SPEC = test/cover.spec
CTLOGDIR = logs
CTFLAGS = \
	-noinput \
	-batch \
	-cover test/cover.spec \
	-pa `pwd`/ebin \
	-dir test \
	-logdir $(CTLOGDIR)

.PHONY: all
all:
	@$(ERL) -make

.PHONY: clean
clean:
	@rm -rf ebin/*.beam logs test/*.beam

.PHONY: dialyze
dialyze: all $(PLT)
	@$(DIALYZER) --plt $(PLT) -r ebin

.PHONY: plt
plt: $(PLT)

$(PLT):
	@$(DIALYZER) --build_plt --apps erts kernel stdlib compiler --output_plt $@

.PHONY: test
test: all $(CTLOGDIR)
	@$(CT_RUN) $(CTFLAGS) -suite test/*_SUITE.erl

$(CTLOGDIR):
	@mkdir $@
