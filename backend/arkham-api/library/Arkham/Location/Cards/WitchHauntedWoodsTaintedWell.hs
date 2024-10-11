module Arkham.Location.Cards.WitchHauntedWoodsTaintedWell (
  witchHauntedWoodsTaintedWell,
  WitchHauntedWoodsTaintedWell (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype WitchHauntedWoodsTaintedWell = WitchHauntedWoodsTaintedWell LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

witchHauntedWoodsTaintedWell :: LocationCard WitchHauntedWoodsTaintedWell
witchHauntedWoodsTaintedWell =
  location
    WitchHauntedWoodsTaintedWell
    Cards.witchHauntedWoodsTaintedWell
    3
    (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsTaintedWell where
  getModifiersFor (InvestigatorTarget iid) (WitchHauntedWoodsTaintedWell a) = maybeModified a do
    lid <- MaybeT $ field InvestigatorLocation iid
    if lid == a.id
      then pure [CanCommitToSkillTestPerformedByAnInvestigatorAt "Witch-Haunted Woods"]
      else do
        isWitchHauntedWoods <- elem lid <$> select (LocationWithTitle "Witch-Haunted Woods")
        guard isWitchHauntedWoods
        pure [CanCommitToSkillTestPerformedByAnInvestigatorAt (be a)]
  getModifiersFor _ _ = pure []

instance RunMessage WitchHauntedWoodsTaintedWell where
  runMessage msg (WitchHauntedWoodsTaintedWell attrs) =
    WitchHauntedWoodsTaintedWell <$> runMessage msg attrs
