module Arkham.Location.Cards.BrokenSteps_290 (brokenSteps_290) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.BlackStarsRise.Helpers
import Arkham.Trait

newtype BrokenSteps_290 = BrokenSteps_290 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenSteps_290 :: LocationCard BrokenSteps_290
brokenSteps_290 = location BrokenSteps_290 Cards.brokenSteps_290 3 (Static 0)

instance HasAbilities BrokenSteps_290 where
  getAbilities (BrokenSteps_290 a) = extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage BrokenSteps_290 where
  runMessage msg l@(BrokenSteps_290 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasAssets <- selectAny $ assetControlledBy iid <> DiscardableAsset
      mCultistCard <-
        find (`cardMatch` (CardWithTrait Cultist <> CardWithType EnemyType))
          <$> scenarioField ScenarioDiscard
      chooseOneM iid do
        when hasAssets do
          withI18n $ countVar 1 $ labeled' "discardAssets" $ chooseAndDiscardAsset iid (attrs.ability 1)
        for_ mCultistCard \c ->
          scenarioI18n $ labeled' "brokenSteps.cultist" $ findAndDrawEncounterCard iid (CardWithId c.id)
      pure l
    _ -> BrokenSteps_290 <$> liftRunMessage msg attrs
