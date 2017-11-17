module Control.Permutation.Proofs

import Control.Permutation.Types

-- identity : (p1 : Permutation n) -> p1 = neutral <+> p1
-- identity {n=Z} Nil = Refl
-- identity {n=S _} _ = ?hole

-- associative : (p1 : Permutation n) -> (p2 : Permutation n) -> (p3 : Permutation n) -> p1 <+> (p2 <+> p3) = (p1 <+> p2) <+> p3
-- associative = ?proof_hole
