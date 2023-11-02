module Roman
    ( RomanNumeral
    , RomanDigit(..)
    , expand
    , normalize
    , romanNumeral
    , (+)
    ) where

import Prelude hiding ((+))
import Data.Function ((&))
import Data.List (sort)

data RomanDigit = I | V | X | L | C | D | M deriving (Eq, Ord, Enum, Read, Show)
type RomanNumeral = [RomanDigit]

romanNumeral :: String -> RomanNumeral
romanNumeral = map (read.(:[]))

(+) :: RomanNumeral -> RomanNumeral -> RomanNumeral
a + b = expand a ++ expand b
      & reverse.sort
      & normalize

isRepeatable :: RomanDigit -> Bool
isRepeatable I = True
isRepeatable V = False
isRepeatable X = True
isRepeatable L = False
isRepeatable C = True
isRepeatable D = False
isRepeatable M = True

expand :: RomanNumeral -> RomanNumeral
expand [I,I,X]     = [V,I,I,I]
expand [I,I,I,C]   = [L,X,X,X,X] ++ [V,I,I]
expand [I,I,C]     = [L,X,X,X,X] ++ [V,I,I,I]
expand [I,C]       = [L,X,X,X,X] ++ [V,I,I,I,I]
expand [I,I,I,X,X] = [X,V,I,I]
expand [I,I,X,X]   = [X,V,I,I,I]
expand [I,X,X]     = [X,V,I,I,I,I]

expand (i:v : rest) | v > i && (not . isRepeatable) v = [i,i,i,i]          ++ expand rest
expand (i:x : rest) | x > i && isRepeatable x         = succ i : [i,i,i,i] ++ expand rest

expand (a : rest) = a : expand rest
expand []         = []

normalize :: RomanNumeral -> RomanNumeral
normalize = use M . use D . use C . use L . use X . use V

use :: RomanDigit -> RomanNumeral -> RomanNumeral
use V (I:I:I:I:I : rest) = V   : use V rest
use V (I:I:I:I   : rest) = I:V : use V rest
use L (X:X:X:X:X : rest) = L   : use L rest
use L (X:X:X:X   : rest) = X:L : use L rest
use D (C:C:C:C:C : rest) = D   : use D rest
use D (C:C:C:C   : rest) = C:D : use D rest

use X (V:V   : rest) = X   : use X rest
use X (V:I:V : rest) = I:X : use X rest
use C (L:L   : rest) = C   : use C rest
use C (L:X:L : rest) = X:C : use C rest
use M (D:D   : rest) = M   : use M rest
use M (D:C:D : rest) = C:M : use M rest

use a (b : rest) = b : use a rest
use _ []         = []
