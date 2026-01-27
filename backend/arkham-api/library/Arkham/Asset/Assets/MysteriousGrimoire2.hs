module Arkham.Asset.Assets.MysteriousGrimoire2 (mysteriousGrimoire2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers (getAdditionalSearchTargets)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype MysteriousGrimoire2 = MysteriousGrimoire2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousGrimoire2 :: AssetCard MysteriousGrimoire2
mysteriousGrimoire2 = asset MysteriousGrimoire2 Cards.mysteriousGrimoire2

instance HasAbilities MysteriousGrimoire2 where
  getAbilities (MysteriousGrimoire2 a) =
    [controlled a 1 (youExist can.search.deck) $ freeTrigger $ UseCostUpTo (be a) Secret 1 2]

instance RunMessage MysteriousGrimoire2 where
  runMessage msg a@(MysteriousGrimoire2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck $ if n == 1 then 3 else 6] #any (defer attrs IsDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      doStep (1 + additionalTargets) msg
      pure a
    DoStep 0 (SearchFound iid (isTarget attrs -> True) _ cards) | notNull cards -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) cards
      when (notNull weaknesses) $ do
        chooseOneAtATimeM iid $ targets weaknesses $ drawCardFrom iid iid
      pure a
    DoStep n (SearchFound iid t@(isTarget attrs -> True) deck cards) | notNull cards -> do
      let (weaknesses, other) = partition (`cardMatch` WeaknessCard) cards
      chooseOneM iid do
        targets weaknesses \card -> do
          drawCardFrom iid iid card
          doStep n (SearchFound iid t deck $ deleteFirst card cards)
        targets other \card -> do
          drawCardFrom iid iid card
          doStep (n - 1) (SearchFound iid t deck $ deleteFirst card cards)
      pure a
    _ -> MysteriousGrimoire2 <$> liftRunMessage msg attrs
