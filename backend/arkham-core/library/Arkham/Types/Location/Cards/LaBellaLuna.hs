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
  deriving newtype (Show, ToJSON, FromJSON, Entity)

laBellaLuna :: LaBellaLuna
laBellaLuna = LaBellaLuna $ baseAttrs
  "02070"
  (Name "La Bella Luna" Nothing)
  EncounterSet.TheHouseAlwaysWins
  2
  (PerPlayer 1)
  Moon
  [Circle]
  [Arkham]

instance HasModifiersFor env LaBellaLuna where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility (Just Action.Resign) (ActionCost 1))

instance ActionRunner env => HasActions env LaBellaLuna where
  getActions iid NonFast (LaBellaLuna attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (LaBellaLuna attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env LaBellaLuna where
  runMessage msg l@(LaBellaLuna attrs) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ unshiftMessage (Resign iid)
    _ -> LaBellaLuna <$> runMessage msg attrs
