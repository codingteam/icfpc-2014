-- |A Sexpr is an S-expressionin the style of Rivest's Canonical
-- S-expressions.  Atoms may be of any type, but String and
-- ByteString have special support.  Rivest's implementation of
-- S-expressions is unusual in supporting MIME type hints for each
-- atom.  See http://people.csail.mit.edu/rivest/Sexp.txt

module Codec.Sexpr (-- * Basics
                             Sexpr,
                             isAtom,
                             isList,
                             atom,
                             list,
                             unAtom,
                             unList,
                             -- * Hinted Atoms
                             hintedAtom,
                             hint,
                             defaultHint,
                             -- * Character predicates to support encoding
                             isTokenChar,isInitialTokenChar,isQuoteableChar,
                             -- * Transformations
                             fold,
                             -- * String printers
                             canonicalString,
                             basicString,
                             advancedString,
                             -- * ShowS printers
                             canonical,
                             -- * Doc pretty printers
                             basic,
                             advanced,
                             -- * Put binary printers
                             putCanonical, putCanonicalBS,
                             -- * Parsers
                             readSexpr,
                             readSexprString,
                             readCanonicalSexprString,
                             advancedSexpr,
                             canonicalSexpr
                             ) where

import Codec.Sexpr.Internal
import Codec.Sexpr.Parser
import Codec.Sexpr.Printer
