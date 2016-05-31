{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- Embedding locking (or scoping?) rules within haskell type system scope is
-- embedded as the identity (co)monad indexed by variables in scope/locks held
module Lib where


import GHC.TypeLits

newtype Lock (s :: Symbol) a = Lock { unLock :: a }
data HList :: [*] -> * where
  Nil      :: HList '[]
  Cons     :: a -> HList b -> HList (a ': b)


newtype LockL (a :: [*]) = L { unL :: HList a }

type family AND (a :: Bool) (b :: Bool) where
  AND 'True 'True   = 'True
  AND 'True 'False  = 'False
  AND 'False 'True  = 'False
  AND 'False 'False = 'False

type family OR (a :: Bool) (b :: Bool) where
  OR 'True 'True   = 'True
  OR 'True 'False  = 'True
  OR 'False 'True  = 'True
  OR 'False 'False = 'False

type family IsMember (a :: k) (l :: [k]) where
  IsMember a '[]      = 'False
  IsMember a (a ': l) = 'True
  IsMember a (x ': l) = IsMember a l

type family IsIn (a :: [k]) (l :: [k]) where
  IsIn '[] '[]      = 'True
  IsIn (a ': as) '[] = 'False
  IsIn (a ': as) (l ': ls) = AND (IsMember a (l ': ls)) (IsIn as (l ': ls))

-- order-preserving subsequence
type family SubSeq (a :: [k]) (l :: [k]) where
  SubSeq '[] '[]             = 'True
  SubSeq '[] (l ': ls)       = 'True
  SubSeq (a ': as) '[]       = 'False
  SubSeq (a ': as) (a ': ls) = SubSeq as ls
  SubSeq (a ': as) (x ': ls) = SubSeq (a ': as) ls

type family Remove (a :: k) (l :: [k]) where
  Remove a '[]      = '[]
  Remove a (a ': l) = Remove a l
  Remove a (x ': l) = x ': (Remove a l)

type family Append (a :: [k]) (l :: [k]) where
  Append '[] a = a
  Append (a ': l) b = a ': (Append l b)

type a +++ b = Append a b

type a <<< b = ((SubSeq a b) ~ 'True)

type Has a b  = ((IsMember a b) ~ 'True)
type NotIn a b  = ((IsMember a b) ~ 'False)

newtype Identity (ls :: [*]) l = Identity { unId :: l }

-- not to export
castList :: Identity a l -> Identity b l
castList (Identity i) = Identity i


-- An indexed (Co)Monad, where each primitive uses the [*] parameter
-- appropriately, given that it represents a lock set
class CompComonad (m :: [*] -> * -> *) where
  iextend  :: (ls' <<< ls) => m ls a -> (m ls' a -> b) -> m ls b
  iextract :: m '[] a -> a
  ireturn  :: a -> m '[] a
  ibind    :: m ls a -> (a -> m ls' b) -> m (ls' +++ ls) b

instance CompComonad Identity where
  iextend i f          = Identity $ f $ castList i
  iextract             = unId
  ireturn              = Identity
  ibind (Identity i) f = castList $ f i

-- Locked resource is a model of a "singleton type" (like stdin, or zfs config
-- locks) for which we have some notion of exclusive access
class CompComonad m => LockedResource l m | l -> m where
  type Contents l
  -- monadic methods
  acquire :: l -> m (l ': '[]) ()
  release :: Has l locks => m locks a -> l -> m (Remove l locks) a

  -- comonadic methods
  readL    :: Has l locks => m locks b -> l -> Contents l

-- Implementing a state (co)monad needed, then locks can become strefs
class LockedResource l m => LockedState l m | l -> m where
  write :: Has l locks => m locks b -> l -> Contents l -> m locks ()

instance LockedResource (Lock s a) Identity where
  type Contents (Lock s a) = a

  acquire _              = Identity ()
  release (Identity a) _ = Identity a
  readL    _ (Lock b)    = b

scoped :: (Has l ls', NotIn l ls, LockedResource l m)
       => l
       -> m ls a
       -> ((m (l ': ls) a) -> m ls' b)
       -> m (Remove l ls') b
scoped l m f = release (f (m                `ibind`
                          (\a  -> acquire l `ibind`
                          (\() -> ireturn a)))) l

test1 :: Integer
test1 = iextract step3
  where l1 :: Lock "l1" Integer
        l1 = Lock 2
        step3 :: Identity '[] Integer
        step3 = release (step2 `ibind` (\n -> ireturn $ n + 1)) l1
        step2 :: Identity '[Lock "l1" Integer] Integer
        step2 = iextend step1 rl
          where rl :: Identity '[Lock "l1" Integer] a -> Integer
                rl = (`readL` l1)
        step1 :: Identity '[Lock "l1" Integer] ()
        step1 = acquire l1

--TODO: serialize access to stdin, stdout etc. as a toy example
-- potentially use TH to do this?
