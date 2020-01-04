SOURCES = $(shell find . -name "*.elm")
HTMLS = $(SOURCES:.elm=.html)

all: $(HTMLS)
	@echo Everything done, sir

%.html: %.elm
	elm make $< --output ../out/$(shell echo $@ | tr '[:upper:]' '[:lower:]') --optimize
