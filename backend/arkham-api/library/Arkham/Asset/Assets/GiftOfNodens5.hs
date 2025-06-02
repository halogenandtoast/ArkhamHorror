module Arkham.Asset.Assets.GiftOfNodens5 (giftOfNodens5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype GiftOfNodens5 = GiftOfNodens5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

giftOfNodens5 :: AssetCard GiftOfNodens5
giftOfNodens5 = asset GiftOfNodens5 Cards.giftOfNodens5

instance HasAbilities GiftOfNodens5 where
  getAbilities (GiftOfNodens5 x) =
    [ playerLimit PerRound
        $ restricted x 1 ControlsThis
        $ triggered (CommittingCardsFromHandToSkillTestStep #when You)
        $ HandDiscardCost 1 #any
        <> ChooseExtendedCardCost (CommittableCard You $ InDiscardOf You <> basic (#skill <> #survivor))
    ]

instance RunMessage GiftOfNodens5 where
  runMessage msg a@(GiftOfNodens5 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenCardPayment -> Just cid) -> do
      withSkillTest \sid ->
        skillTestModifiers sid (attrs.ability 1) cid [PlaceOnBottomOfDeckInsteadOfDiscard, MustBeCommitted]
      commitCard iid =<< getCard cid
      pure a
    _ -> GiftOfNodens5 <$> liftRunMessage msg attrs
