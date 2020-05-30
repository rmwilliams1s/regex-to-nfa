# Regex-to-Nfa

Regex-to-Nfa is a Haskell project implementing the regular expression to nondeterministic automata (NFA) [conversion algorithm](https://www.geeksforgeeks.org/regular-expression-to-nfa/). 

## Installation

Use the build tool [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) to run Regex-to-Nfa.

```bash
stack build
```

## Usage

```bash
stack run regex-to-nfa -- runs project on multiple given examples

stack ghci
:l main.hs
let re = "(ab)*a|c"
testOne re -- prints NFA for regex given above
```

## Known Bugs

Matching capability in progress, not fully implemented

## Support 

Email rwilliams1@gm.slc.edu with any known issues.

## Roadmap

Eventually aim to implement matching (i.e. is a given string accepted in the regex's language), conversion from NFA to DFA, and pretty printing for NFAs and DFAs alike.

## Contributing

Pull requests are welcome.

## Authors and Acknowledgement

Written by Rachel Williams, with the help of Michael Siff.


