module Arkham.Location.Cards.TheCrossroadsNight (theCrossroadsNight) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FateOfTheVale.Helpers
import Arkham.SkillTest
import Arkham.SkillTestResult
import Arkham.Trait (Trait (Shattered))

newtype TheCrossroadsNight = TheCrossroadsNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCrossroadsNight :: LocationCard TheCrossroadsNight
theCrossroadsNight = symbolLabel $ location TheCrossroadsNight Cards.theCrossroadsNight 2 (PerPlayer 2)

instance HasModifiersFor TheCrossroadsNight where
  getModifiersFor (TheCrossroadsNight attrs) =
    modifySelect
      attrs
      (EnemyAt (be attrs) <> EnemyWithTrait Shattered)
      [CriteriaModifier (DuringPhase IsEnemyPhase) $ RemoveKeyword Aloof]

instance HasAbilities TheCrossroadsNight where
  getAbilities (TheCrossroadsNight a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (exists $ EnemyAt (be a) <> EnemyWithTrait Shattered)
      $ freeReaction
      $ EnemyEvadedSuccessfully #after You AnySource
      $ EnemyAt (be a)
      <> EnemyWithTrait Shattered

instance RunMessage TheCrossroadsNight where
  runMessage msg l@(TheCrossroadsNight attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      getSkillTest >>= traverse_ \st -> case skillTestResult st of
        SucceededBy _ n | n >= 3 -> whenFateOfTheValeV4 $ remember TheRoadIsClear
        _ -> pure ()
      pure l
    _ -> TheCrossroadsNight <$> liftRunMessage msg attrs
