# dynasty

dynasty is a program for identifying the topologies (or integral
families) of Feynman graphs. It is based on [nauty and
Traces](http://pallini.di.uniroma1.it/).

## Installation

If [Rust and Cargo](https://www.rust-lang.org/) are installed on your
system, run

    cargo install dynasty

Precompiled executables are available on
[github](https://github.com/a-maier/dynasty).

## Usage

dynasty reads in Feynman diagrams in a [YAML](https://yaml.org/) or
[FORM](https://github.com/vermaseren/form)-based format and, for each
diagram, prints its topology and how loop momenta have to be shifted
to obtain a uniform assignment of propagator momenta.

Basic usage is

  dynasty -o outfile.yml diagrams.yml

It is possible to pass more than one input file, for instance

  dynasty -o outfile.yml topologies.yml diagrams.yml

to ensure the diagrams in `diagrams.yml` are mapped onto the
topologies defined in `topologies.yml` as far as possible.

### Important Options

* Use the `-s` flag to allow mapping onto subtopologies, i.e. onto
  diagrams where one or more propagators have been contracted.

* Use `-f` to switch between YAML and FORM output.

* `--replace-masses` and `--replace-momenta` can be used for kinematic
  replacements, e.g. `--replace-momenta='{p1: q, p2: q}'` to set the
  momenta `p1` and `p2` to `q`.

### Input format

#### YAML

The input in YAML format has the form
```yaml
diagram0:
  - [from0, to0, p0, m0] # first propagator
  - [from1, to1, p1, m1] # second propagator
  # further propagators ...
---
diagram1:
  # propagators ...
```
`from` and `to` are non-negative integer vertex labels designating
the start and end of the propagator line. `p` is the propagator
momentum (e.g. `l1 + q`) and `m` its mass. It is allowed to omit
both `from` and `to` and the same time to denote a scalar product
that is not associated with a graph edge. `dynasty` will ignore
such scalar products.

The document separator `---` is optional but recommended for large
input files to save memory.

dynasty includes a
[QGRAF](http://cfif.ist.utl.pt/~paulo/qgraf.html) style file
`share/qgraf/yaml.sty` that generates the required input. If
several fields have the same mass the corresponding masses in the
QGRAF output have to be adjusted manually.

#### FORM

FORM input files should start with the following line:
```rust
* dynasty-format: FORM
```
The remainder of the file should consist of diagram folds:
```rust
*--#[ DIANAME:
  diagram specification
*--#] DIANAME:
```
In the diagram specification, only propagator lines of the form
`*prop(from, to, field(fieldnr, ...))` with arbitrary extra
whitespace are interpreted. The ellipsis indicates any number of
further arguments. Only the `from`, `to`, and `field`
specifications are considered for the topology mapping, and
`field` is interpreted as a mass. Use the `--replace-masses`
option to replace fields by actual masses.

### Output formats

#### YAML

The default YAML output consists of records
```yaml
diagram: [topology, {l1: p1, ...}]
```
where `diagram` is the diagram name and `topology` the first
passed diagram with the same topology. The last part of the entry
indicates how loop momenta have to be replaced to arrive at the
same momentum assignment as in `topology`.

#### FORM

Alternatively `-f form` to produce output for FORM. In this case,
*mapping information is only written for diagrams that have integer
numbers as names*. Then, an output file `topologies.frm` can be
used as follows:
```FORM
cf dia;
#include- topologies.frm
* example diagram
* the argument of `dia` should match the name in the dynasty input file
local diagrams =
+ dia(1)
* ... diagram information (propagators, vertices)
+ dia(2)
* ... more diagrams
;

id dia(?a) = dia(?a) * topology(?a);
if(match(top(?a$a)) && match(top(?b$b)));
   print "the topology of diagram %$ is %$" $a $b;
endif;
* shift momenta to canonical form
id replace(?a) = replace_(?a);
.end
```

License: GPL-3.0-or-later
