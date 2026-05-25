module Arkham.Location.Cards.CoveredBridge (coveredBridge) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype CoveredBridge = CoveredBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coveredBridge :: LocationCard CoveredBridge
coveredBridge = locationWith CoveredBridge Cards.coveredBridge 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CoveredBridge where
  getAbilities (CoveredBridge a) =
    extendRevealed1 a
      $ restricted a 1 (DuringSkillTest $ WhileInvestigating $ be a)
      $ forced
      $ RevealChaosToken #after You
      $ mapOneOf ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail]

instance RunMessage CoveredBridge where
  runMessage msg l@(CoveredBridge attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.openWater10599b
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> CoveredBridge <$> liftRunMessage msg attrs
