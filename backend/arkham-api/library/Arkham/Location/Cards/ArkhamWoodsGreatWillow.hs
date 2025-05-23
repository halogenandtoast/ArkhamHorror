module Arkham.Location.Cards.ArkhamWoodsGreatWillow (arkhamWoodsGreatWillow) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message (gainSurge)
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Location.Cards qualified as Cards (arkhamWoodsGreatWillow)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: LocationCard ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow =
  location ArkhamWoodsGreatWillow Cards.arkhamWoodsGreatWillow 4 (PerPlayer 1)

instance HasAbilities ArkhamWoodsGreatWillow where
  getAbilities (ArkhamWoodsGreatWillow attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 Here
      $ forced
      $ SkillTestResult #after You (SkillTestOnTreachery AnyTreachery) #success

instance RunMessage ArkhamWoodsGreatWillow where
  runMessage msg l@(ArkhamWoodsGreatWillow attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      void $ runMaybeT do
        source <- MaybeT getSkillTestSource
        tid <- hoistMaybe source.treachery
        lift $ push $ gainSurge tid
      pure l
    _ -> ArkhamWoodsGreatWillow <$> liftRunMessage msg attrs
