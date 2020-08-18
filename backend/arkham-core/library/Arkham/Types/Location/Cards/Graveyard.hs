{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Graveyard where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype Graveyard = Graveyard Attrs
  deriving newtype (Show, ToJSON, FromJSON)

graveyard :: Graveyard
graveyard =
  Graveyard $ (baseAttrs "01133" "Graveyard" 1 (PerPlayer 2) Hourglass [Circle])
    { locationTraits = HashSet.fromList [Arkham]
    , locationVictory = Just 1
    }

instance HasActions Graveyard where
  getActions (Graveyard attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env Graveyard where
  runMessage msg (Graveyard attrs) = Graveyard <$> runMessage msg attrs
