module Arkham.Asset.Assets.ScrollOfThePharaohsWordsOfSet4 (scrollOfThePharaohsWordsOfSet4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype ScrollOfThePharaohsWordsOfSet4 = ScrollOfThePharaohsWordsOfSet4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfThePharaohsWordsOfSet4 :: AssetCard ScrollOfThePharaohsWordsOfSet4
scrollOfThePharaohsWordsOfSet4 = asset ScrollOfThePharaohsWordsOfSet4 Cards.scrollOfThePharaohsWordsOfSet4

instance HasAbilities ScrollOfThePharaohsWordsOfSet4 where
  getAbilities (ScrollOfThePharaohsWordsOfSet4 a) =
    [ controlled_ a 1
        $ investigateAction
        $ HorrorCost (toSource a) YouTarget 1
        <> UseCostUpTo (be a) Secret 1 2
    ]

instance RunMessage ScrollOfThePharaohsWordsOfSet4 where
  runMessage msg a@(ScrollOfThePharaohsWordsOfSet4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #intellect 3, DiscoveredClues n]
      investigate sid iid source
      pure a
    _ -> ScrollOfThePharaohsWordsOfSet4 <$> liftRunMessage msg attrs
