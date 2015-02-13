{-# LANGUAGE DeriveGeneric, TypeFamilies, TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Control.Lens.Generic
    ( lenses, LensFor(..) ) where

import Data.Functor
import Data.Typeable

import GHC.Generics

type GenericLensesFor m a = GenericLenses a m (Rep (a m))
newtype LensFor a c x = LensFor (forall f. Functor f => (a x -> f (a x)) -> c a -> f (c a))

type family GenericLenses c m a where
    GenericLenses c m (D1 d a) = D1 d (GenericLenses c m a)
    GenericLenses c m (C1 d a) = C1 d (GenericLenses c m a)
    GenericLenses c m (S1 d a) = S1 d (GenericLenses c m a)
    GenericLenses c m (K1 R (m x)) = K1 R (LensFor m c x)
    GenericLenses c m (a :*: b) = GenericLenses c m a :*: GenericLenses c m b
    GenericLenses c m U1 = U1

class GLenses c m a where
    glenses :: (forall f. Functor f => (a p -> f (a p)) -> c m -> f (c m)) -> GenericLenses c m a p
instance GLenses c m a => GLenses c m (D1 d a) where
    glenses (set :: forall f. Functor f => (D1 d a p -> f (D1 d a p)) -> c m -> f (c m)) = M1 $ glenses (\modifier -> set ((M1 <$>) . modifier . unM1)) :: D1 d (GenericLenses c m a) p
instance GLenses c m a => GLenses c m (C1 d a) where
    glenses (set :: forall f. Functor f => (C1 d a p -> f (C1 d a p)) -> c m -> f (c m)) = M1 $ glenses (\modifier -> set ((M1 <$>) . modifier . unM1)) :: C1 d (GenericLenses c m a) p
instance GLenses c m a => GLenses c m (S1 d a) where
    glenses (set :: forall f. Functor f => (S1 d a p -> f (S1 d a p)) -> c m -> f (c m)) = M1 $ glenses (\modifier -> set ((M1 <$>) . modifier . unM1)) :: S1 d (GenericLenses c m a) p
instance (GLenses c m a, GLenses c m b) => GLenses c m (a :*: b) where
    glenses (set :: forall f. Functor f => ((a :*: b) p -> f ((a :*: b) p)) -> c m -> f (c m)) = glenses modifyLeft :*: glenses modifyRight :: GenericLenses c m (a :*: b) p
        where modifyLeft :: forall f. Functor f => (a p -> f (a p)) -> c m -> f (c m)
              modifyLeft modifier = set (\(a :*: b) -> (:*: b) <$> modifier a)
              modifyRight :: forall f. Functor f => (b p -> f (b p)) -> c m -> f (c m)
              modifyRight modifier = set (\(a :*: b) -> (a :*:) <$> modifier b)
instance GLenses c m (K1 R (m a)) where
    glenses (set :: forall f. Functor f => (K1 R (m a) p -> f (K1 R (m a) p)) -> c m -> f (c m)) = K1 (LensFor modify)
        where modify :: forall f. Functor f => (m a -> f (m a)) -> c m -> f (c m)
              modify modifier = set ((K1 <$>) . modifier . unK1)

lenses' :: ( GLenses a m (Rep (a m))
              , Generic (a m), Generic (a (LensFor m a))
              , GenericLensesFor m a ~ Rep (a (LensFor m a)) ) => Proxy a -> Proxy m -> a (LensFor m a)
lenses' (_ :: Proxy a) (_ :: Proxy m) = to (glenses modify)
    where modify :: forall f. Functor f => (Rep (a m) ()  -> f (Rep (a m) ())) -> a m -> f (a m)
          modify modifier = (to <$>) . modifier . from

lenses ::  ( GLenses a m (Rep (a m))
              , Generic (a m), Generic (a (LensFor m a))
              , GenericLensesFor m a ~ Rep (a (LensFor m a)) ) => a (LensFor m a)
lenses = inj lenses'
    where inj :: (Proxy a -> Proxy m -> a (LensFor m a)) -> a (LensFor m a)
          inj f = f Proxy Proxy
