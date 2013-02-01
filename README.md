Nerf
====

Nerf is a statistical named entity recognition (NER) tool based on linear-chain
conditional random fields (CRFs).
It has been adapted to recognize tree-like structures of NEs (i.e., with
recursively embedded NEs) by using the joined label tagging method which
-- for a particular sentence -- works as follows:

  * CRF model is used to determine the most probable sequence of labels,
  * Extended IOB method is used to decode the sequence into a forerst of NEs.

The extended IOB method also provides the inverse encoding function which is
needed during the model training.

Building Nerf
=============

You will need the [Glasgow Haskell Compiler](http://www.haskell.org/ghc/)
and the [Cabal](http://www.haskell.org/cabal/) tool to build Nerf.
The easiest way to get both GHC and Cabal is to install the latest
[Haskell Platform](http://www.haskell.org/platform/).

To install Nerf from the official
[Hackage](http://hackage.haskell.org/package/nerf)
repository just run:

    cabal install nerf

If you want to update Nerf to a newer version you should update the package
list first:

    cabal update
    cabal install nerf

To install the latest development version from github just run

    cabal install

from the `nerf` top level directory.

Data format
===========

Annotated data
--------------

The current version of Nerf works with a simple data format in which:

  * Each sentence is kept in a separate line,
  * Named entities are represented with embedded beginning and ending tags,
  * Contents of individual tags represent named entity types.

For example:

    <organization>Church of the <deity>Flying Spaghetti Monste</deity></organization>

Text and label values should be escaped by prepending the `\` character before special
`>`, `<`, `\` and ` ` (space) characters.

NER input data
--------------

When the tool is used in the NER mode each sentence should be in a separate line.
It's because Nerf currently doesn't perform any sentence level segmentation.

Training
========

Once you have an annotated data file `train.nes` (and, optionally, an evaluation
material `eval.nes`) conformant with the format described above you can train
the Nerf model using the following command:

    nerf train train.nes -e eval.nes -o model.bin

Run `nerf train --help` to learn more about the program arguments and possible
training options.

The nerf tool can be also supplied with additional 
[runtime system options](http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html).
For example, to train the model using four threads, use:

    nerf train train.nes -e eval.nes -o model.bin +RTS -N4

Dictionaries
------------

Nerf supports a list of NE-related dictionaries:

  * [PoliMorf](\url{http://zil.ipipan.waw.pl/PoliMorf}),
  * [NELexicon](\url{http://nlp.pwr.wroc.pl/en/tools-and-resources/nelexicon}),
  * [Gazetteer for Polish Named Entities](\url{http://clip.ipipan.waw.pl/Gazetteer}),
  * [PNET](\url{http://zil.ipipan.waw.pl/PNET}),
  * [Prolexbase](\url{http://zil.ipipan.waw.pl/Prolexbase}).

To use the particular dictionary during NER you have to supply it as a
command line argument during the training process, for example:

    nerf train train.nes --polimorf PoliMorf-0.6.1.tab

Named entity recognition
========================

To annotate the `input.txt` data file using the trained `model.bin` model, run: 

    nerf ner model.bin input.txt

Anotated data will be printed to `stdout`.  Remember that each sentence in the
input file should be in a separate line.
