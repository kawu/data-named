Data-named
==========

The library provides data types which can be used to represent forest
structures with labels stored in internal nodes and words kept in leaves.  In
particular, those types are well suited for representing the layer of named
entities (NEs).

The IOB method is implemented in the Data.Named.IOB module and can be used to
translate between a forest of entities and a sequence of compound IOB labels.
This method can be used together with a sequence classifier to indirectly model
forest structures.

The Data.Named.Graph module can be used to represent more general, graph
structures of entities.  The module provides also a lossy conversion from a DAG
to a disjoint forest of entities.

[![Build Status](https://travis-ci.org/kawu/data-named.svg)](https://travis-ci.org/kawu/data-named)
