module Arkham.Homebrew.AgainstTheWendigo.Assets.GoldMiningRevenues (goldMiningRevenues) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWith_)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Modifier (setActiveDuringSetup)

newtype GoldMiningRevenues = GoldMiningRevenues AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goldMiningRevenues :: AssetCard GoldMiningRevenues
goldMiningRevenues = asset GoldMiningRevenues Cards.goldMiningRevenues

instance HasModifiersFor GoldMiningRevenues where
  getModifiersFor (GoldMiningRevenues a) = for_ a.controller \iid -> do
    modifiedWith_ a iid setActiveDuringSetup [StartingResources 2]

instance HasAbilities GoldMiningRevenues where
  getAbilities (GoldMiningRevenues a) = [controlled_ a 1 $ forced $ GameBegins #when]

instance RunMessage GoldMiningRevenues where
  runMessage msg a@(GoldMiningRevenues attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      directHorror iid (attrs.ability 1) 1
      pure a
    _ -> GoldMiningRevenues <$> liftRunMessage msg attrs
