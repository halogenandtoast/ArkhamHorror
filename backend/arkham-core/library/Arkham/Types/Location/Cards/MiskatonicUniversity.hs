{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MiskatonicUniversity where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype MiskatonicUniversity = MiskatonicUniversity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicUniversity :: MiskatonicUniversity
miskatonicUniversity = MiskatonicUniversity $ (baseAttrs
                                                "01129"
                                                "Miskatonic University"
                                                4
                                                (PerPlayer 2)
                                                Diamond
                                                [T, Plus, Circle, Square]
                                              )
  { locationTraits = HashSet.fromList [Arkham]
  , locationVictory = Just 1
  }

instance HasActions MiskatonicUniversity where
  getActions (MiskatonicUniversity attrs) iid = getActions attrs iid

instance (LocationRunner env) => RunMessage env MiskatonicUniversity where
  runMessage msg (MiskatonicUniversity attrs) =
    MiskatonicUniversity <$> runMessage msg attrs
