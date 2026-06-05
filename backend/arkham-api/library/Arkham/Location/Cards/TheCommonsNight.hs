module Arkham.Location.Cards.TheCommonsNight (theCommonsNight) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FateOfTheVale.Helpers

newtype TheCommonsNight = TheCommonsNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCommonsNight :: LocationCard TheCommonsNight
theCommonsNight = symbolLabel $ location TheCommonsNight Cards.theCommonsNight 2 (PerPlayer 1)

instance HasModifiersFor TheCommonsNight where
  getModifiersFor (TheCommonsNight attrs) = do
    modifySelect attrs Anywhere [WhileEnemyMovingModifier $ ConnectedToWhen Anywhere (be attrs)]

instance HasAbilities TheCommonsNight where
  getAbilities (TheCommonsNight a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage TheCommonsNight where
  runMessage msg l@(TheCommonsNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 1 $ attrs.ability 1) iid #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (IndexedSource 1 (isAbilitySource attrs 1 -> True)) -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 2 $ attrs.ability 1) iid #combat (Fixed 2)
      pure l
    PassedThisSkillTest iid (IndexedSource 2 (isAbilitySource attrs 1 -> True)) -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 3 $ attrs.ability 1) iid #agility (Fixed 1)
      pure l
    PassedThisSkillTest iid (IndexedSource 3 (isAbilitySource attrs 1 -> True)) -> do
      whenM (can.gain.resources iid) do
        gainResources iid (attrs.ability 1) 5
        whenFateOfTheValeV4 $ remember TheInvestigatorsFoundTheosTruck
      pure l
    _ -> TheCommonsNight <$> liftRunMessage msg attrs
