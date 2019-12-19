MODULES=template action status worldgui battlegui gui main battletest author
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
BATTLETEST=battletest.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,yojson,graphics,camlimages.png,camlimages.graphics,sdl,sdl.sdlmixer

default: build
	utop

build:
	$(OCAMLBUILD) -package $(PKGS) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

battletest:
	$(OCAMLBUILD) $(BATTLETEST) && ./$(BATTLETEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip pokemon.zip *.ml* *.json _tags Makefile
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS), \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS), \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private pokemon.zip
