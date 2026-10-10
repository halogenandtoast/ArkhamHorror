{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by the Ages Unwound campaign.

Derived mechanically from @real_traits@ across
@docs/homebrew/data/ages-unwound-mapped.json@ (both sides of all 257 cards),
minus everything 'Arkham.Trait' already defines. 'declareHomebrewTraits'
generates a bidirectional pattern synonym per name plus the aggregate @traits@
list, which @Defs.hs@ folds into the global trait universe. Compilation fails
if a name here has graduated into core — @Paradox@ and @Task@ are both core
traits already and are deliberately absent.

One printed trait has no entry: @Arkham?@, on both printings of /A Disquieting
Future/ (@:ages-unwound:069@, @:ages-unwound:070@). It is not a legal
constructor and a trait of its own would not match the campaign's many
@[[Arkham]]@ references, so those cards carry the core @Arkham@ trait and lose
the printed question mark.
-}
module Arkham.Homebrew.AgesUnwound.Traits (module Arkham.Homebrew.AgesUnwound.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "Adrift"
  , "Army"
  , "Cretaceous"
  , "Darkness"
  , "Egypt"
  , "Exterior"
  , "Fate"
  , "France"
  , "Garden"
  , "Interior"
  , "Library"
  , "Myriad"
  , "Rome"
  , "Sorrow"
  , "Sphinx"
  , "Tatterdemalion"
  , "Temporal"
  , "Yeti"
  ]
