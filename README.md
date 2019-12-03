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


Installation
=============

It is recommanded to install *nerf* using the
[Haskell Tool Stack][stack], which you will need to downoload and
install on your machine beforehand.  Then clone this repository into
a local directory and use `stack` to install the library by running:

    stack install


Data formats
============

The only data encoding supported by Nerf is `UTF-8`.

Training data
-------------

The current version of Nerf works with a simple data format in which:

  * Each sentence is kept in a separate line,
  * Named entities are represented with embedded beginning and ending tags,
  * Contents of individual tags represent named entity types.

For example:

    <organization>Church of the <deity>Flying Spaghetti Monster</deity></organization> .

Text and label values should be escaped by prepending the `\` character before special
`>`, `<`, `\` and ` ` (space) characters.

Have a look in the `example` directory for an example of a file in the
appropriate format.

NER input data
--------------

Below is a list of data formats supported within the NER mode.

### Raw text

Nerf can be used to annotate raw text with named entites.  The annotated data
will be presented in the format which is also used for training and has already
been described above.  Each sentence should be supplied in a separate line --
currently, Nerf doesn't perform any sentence-level segmentation.

### XCES format

It is also possible to annotate data stored in the XCES format.


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

**WARNING**: Currently, the `-N` runtime option sometimes leads to errors in
the training process and therefore should be avoided for the time being.


Dictionaries
------------

Nerf supports a list of NE-related dictionaries:

  * [PoliMorf](http://zil.ipipan.waw.pl/PoliMorf),
  * [NELexicon](http://nlp.pwr.wroc.pl/en/tools-and-resources/nelexicon),
  * [Gazetteer for Polish Named Entities](http://clip.ipipan.waw.pl/Gazetteer),
  * [PNET](http://zil.ipipan.waw.pl/PNET),
  * [Prolexbase](http://zil.ipipan.waw.pl/Prolexbase).

To use the particular dictionary during NER you have to supply it as a
command line argument during the training process, for example:

    nerf train train.nes --polimorf PoliMorf-0.6.1.tab


Named entity recognition
========================

To annotate the `input.txt` data file using the trained `model.bin` model, run: 

    nerf ner model.bin < input.txt

Annotated data will be printed to `stdout`.  Data formats currently supported within
the NER mode has been described above.  Run `nerf ner --help` to learn more about the
additional NER arguments.


Server
======

Nerf provides also a client/server mode.  It is handy when, for example,
you need to annotate a large collection of small files.  Loading Nerf model
from a disk takes considerable amount of time which makes the tagging method
described above very slow in such a setting.

To start the Nerf server, run:

    nerf server model.bin

You can supply a custom port number using a `--port` option.  For example,
to run the server on the `10101` port, use the following command:

    nerf server model.bin --port 10101

To use the server in a multi-threaded environment, you need to specify the
`-N` [RTS][ghc-rts] option.  A set of options which usually yield good
server performance is presented in the following example:

    nerf server model.bin +RTS -N -A4M -qg1 -I0

Run `nerf server --help` to learn more about possible server-mode options.

The client mode works just like the tagging mode.  The only difference is that,
instead of supplying your client with a model, you need to specify the port number
(in case you used a custom one when starting the server; otherwise, the default
port number will be used).

    nerf client --port 10101 < input.txt > output.nes

Run `nerf client --help` to learn more about the possible client-mode options.


[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[ghc-rts]: http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html "GHC runtime system options"
