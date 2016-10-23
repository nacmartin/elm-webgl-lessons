SOURCES = lesson01.elm lesson02.elm lesson03.elm lesson04.elm lesson05.elm lesson06.elm lesson07.elm lesson08.elm lesson09.elm lesson10.elm lesson11.elm
HTMLS = $(SOURCES:.elm=.html)

all: $(HTMLS)
	@echo Everything done, sir

%.html: %.elm
	elm-make $< --output ../out/$@
