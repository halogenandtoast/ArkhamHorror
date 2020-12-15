{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.LaBellaLuna
  ( laBellaLuna
  , LaBellaLuna(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype LaBellaLuna = LaBellaLuna Attrs
  deriving newtype (Show, ToJSON, FromJSON)

laBellaLuna :: LaBellaLuna
laBellaLuna = LaBellaLuna $ baseAttrs
  "02070"
  "La Bella Luna"
  EncounterSet.TheHouseAlwaysWins
  2
  (PerPlayer 1)
  Moon
  [Circle]
  [Arkham]

instance HasModifiersFor env LaBellaLuna where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Resign))

instance ActionRunner env => HasActions env LaBellaLuna where
  getActions iid NonFast (LaBellaLuna attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ do
      canAffordActions <- getCanAffordCost
        iid
        (toSource attrs)
        (ActionCost 1 (Just Action.Resign) locationTraits)
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid `member` locationInvestigators && canAffordActions
        ]
  getActions iid window (LaBellaLuna attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env LaBellaLuna where
  runMessage msg (LaBellaLuna attrs) = LaBellaLuna <$> runMessage msg attrs
