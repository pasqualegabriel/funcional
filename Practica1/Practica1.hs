-- Ejercicio 4 
data Nat = Zero | Suc Nat

inc :: Nat -> Nat
inc n = Suc n

add :: Nat -> Nat -> Nat
add n Zero = n
add Zero n = n
add (Suc n1) n2 = add n1 (Suc n2) 
--add (Suc Zero) (Suc (Suc Zero))

sub :: Nat -> Nat -> Maybe Nat
sub Zero Zero = Just Zero
sub n Zero = Just n
sub Zero n = Nothing
sub (Suc n1) (Suc n2) = sub n1 n2
--sub (Suc (Suc Zero)) (Suc Zero)

igual :: Nat -> Nat -> Bool
igual Zero Zero = True
igual Zero (Suc _) = False
igual (Suc _) Zero = False
igual (Suc n1) (Suc n2) = igual n1 n2 
-- igual (Suc (Suc Zero)) (Suc (Suc Zero))

mayorIgual :: Nat -> Nat -> Bool
mayorIgual _ Zero = True
mayorIgual Zero _ = False
mayorIgual (Suc n1) (Suc n2) = mayorIgual n1 n2
-- mayorIgual (Suc (Suc Zero)) (Suc (Suc Zero))