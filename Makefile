# Makefile to compile the project 'Hilbert R-Tree'
# Authors:
#     (c) Victor De Ponte - 05-38087 - <rdbvictor19@gmail.com> 2012,
#     (c) Germán Jaber    - 06-39749 - <plaga701@gmail.com> 2012,

DOCUMENTABLES = RTree.hs

all:
	ghc --make main.hs -o rtree

hoogle:
	haddock --hoogle -o doc/haddock/hoogle ${DOCUMENTABLES}

html:
	haddock -h -o doc/haddock/html ${DOCUMENTABLES} --source-base=RTree.hs

docs:
	haddock -o doc/haddock/docs ${DOCUMENTABLES}

clean:
	rm -rf doc/haddock 2> /dev/null
	rm rtree 2> /dev/null