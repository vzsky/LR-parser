# LR-parser

A trial at writing LR parser using Ocaml. 

Grammar can be defined using English letters, where $S$ is the starting nonterminal, capital letters are reserved for nonterminals and 
small letters are for terminals. 

The grammar will be parsed with LR, but lazily, that is the table will only be generated when needed. Also, the entry will not be 
stored to keep this "functional", so the complexity would be different than the efficient one.

To be compiled with Ocaml 5.2.0
