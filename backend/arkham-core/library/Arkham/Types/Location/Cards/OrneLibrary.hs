{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.OrneLibrary where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype OrneLibrary = OrneLibrary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

orneLibrary :: OrneLibrary
orneLibrary = OrneLibrary $ (baseAttrs
                              "02050"
                              "Orne Library"
                              EncounterSet.ExtracurricularActivity
                              3
                              (PerPlayer 1)
                              Triangle
                              [Plus, Square]
                              [Miskatonic]
                            )
  { locationVictory = Just 1
  }

instance HasModifiersFor env OrneLibrary where
  getModifiersFor _ target (OrneLibrary attrs) | isTarget attrs target =
    pure [ActionCostOf (IsAction Action.Investigate) 1]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env OrneLibrary where
  getActions i window (OrneLibrary attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OrneLibrary where
  runMessage msg (OrneLibrary attrs) = OrneLibrary <$> runMessage msg attrs
