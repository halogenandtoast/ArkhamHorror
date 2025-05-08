module Arkham.Location.Cards.VillageCommonsSilentDecay (villageCommonsSilentDecay) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.BloodOnTheAltar.Helpers

newtype VillageCommonsSilentDecay = VillageCommonsSilentDecay LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

villageCommonsSilentDecay :: LocationCard VillageCommonsSilentDecay
villageCommonsSilentDecay =
  locationWith
    VillageCommonsSilentDecay
    Cards.villageCommonsSilentDecay
    3
    (Static 0)
    (labelL .~ "villageCommons")

instance HasModifiersFor VillageCommonsSilentDecay where
  getModifiersFor (VillageCommonsSilentDecay a) = do
    modifySelect a (investigatorAt a) [CannotTakeAction #draw, CannotTakeAction #resource]

instance HasAbilities VillageCommonsSilentDecay where
  getAbilities (VillageCommonsSilentDecay a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "villageCommons.resign" $ locationResignAction a

instance RunMessage VillageCommonsSilentDecay where
  runMessage msg (VillageCommonsSilentDecay attrs) =
    VillageCommonsSilentDecay <$> runMessage msg attrs
