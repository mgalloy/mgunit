TITLE=testing-with-idl

LATEX=xelatex

.PHONY: clean all

all: $(TITLE).pdf

$(TITLE).pdf: $(TITLE).tex
	$(LATEX) $(TITLE)
	$(LATEX) $(TITLE)

clean:
	rm -f $(TITLE).out $(TITLE).log $(TITLE).toc $(TITLE).aux $(TITLE).nav $(TITLE).snm $(TITLE).pdf
