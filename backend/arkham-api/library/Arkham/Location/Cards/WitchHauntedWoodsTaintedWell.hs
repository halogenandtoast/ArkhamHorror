module Arkham.Location.Cards.WitchHauntedWoodsTaintedWell (
  witchHauntedWoodsTaintedWell,
  WitchHauntedWoodsTaintedWell (..),
) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
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
  getModifiersFor (WitchHauntedWoodsTaintedWell a) =
    whenRevealed a $ modifySelectMaybe a Anyone \iid -> do
      lid <- MaybeT $ field InvestigatorLocation iid
      if lid == a.id
        then pure [CanCommitToSkillTestPerformedByAnInvestigatorAt "Witch-Haunted Woods"]
        else do
          liftGuardM $ lid <=~> LocationWithTitle "Witch-Haunted Woods"
          pure [CanCommitToSkillTestPerformedByAnInvestigatorAt (be a)]

instance RunMessage WitchHauntedWoodsTaintedWell where
  runMessage msg (WitchHauntedWoodsTaintedWell attrs) =
    WitchHauntedWoodsTaintedWell <$> runMessage msg attrs
