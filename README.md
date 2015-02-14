# generic-lens

Automatically make lenses from data structures implementing Generic

## Example Usage (in GHCi)

    > :set -XDeriveGeneric -XStandaloneDeriving -XFlexibleContexts -XFlexibleInstances
    > import Control.Lens.Generic
    > import Control.Monad.Identity
    > import GHC.Generics
    > import Control.Lens
    > data Employee f = Employee (f String) (f Int) deriving Generic
    > let Employee (LensFor name) (LensFor age) = lenses

Now you can do things like:

    > let x = Employee (return "Frank") (return 2) :: Employee Identity
    > runIdentity (x ^. name)
    "Frank"
    > let y = x & name .~ return "Bob"
    > runIdentity (y ^. name)
    "Bob"

You can also define the lenses at the top of your module as expected

    > Employee (LensFor name) (LensFor age) = lenses

The generated lenses have the correct polymorphic van Laarhoven type:

    > :type name
    name
      :: Functor f =>
         (t String -> f (t String)) -> Employee t -> f (Employee t)
