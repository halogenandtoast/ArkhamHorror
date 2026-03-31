module Arkham.Asset.Assets.ScrollOfThePharaohsWordsOfRa4 (scrollOfThePharaohsWordsOfRa4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype ScrollOfThePharaohsWordsOfRa4 = ScrollOfThePharaohsWordsOfRa4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfThePharaohsWordsOfRa4 :: AssetCard ScrollOfThePharaohsWordsOfRa4
scrollOfThePharaohsWordsOfRa4 = asset ScrollOfThePharaohsWordsOfRa4 Cards.scrollOfThePharaohsWordsOfRa4

instance HasAbilities ScrollOfThePharaohsWordsOfRa4 where
  getAbilities (ScrollOfThePharaohsWordsOfRa4 a) =
    [ controlled_ a 1
        $ fightActionWith #intellect
        $ HorrorCost (toSource a) YouTarget 1
        <> UseCostUpTo (be a) Secret 1 2
    ]

instance RunMessage ScrollOfThePharaohsWordsOfRa4 where
  runMessage msg a@(ScrollOfThePharaohsWordsOfRa4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #intellect 3, DamageDealt n]
      chooseFightEnemyWith #intellect sid iid source
      pure a
    _ -> ScrollOfThePharaohsWordsOfRa4 <$> liftRunMessage msg attrs
