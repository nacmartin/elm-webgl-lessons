SOURCES = lesson01.elm lesson02.elm lesson03.elm
HTMLS = $(SOURCES:.elm=.html)

all: $(HTMLS)
	@echo Everything done, sir

%.html: %.elm
	elm-make $< --output ../out/$@
