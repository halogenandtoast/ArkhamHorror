module Arkham.Location.Cards.UptownYeOldeMagickShoppe (uptownYeOldeMagickShoppe) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (uptownYeOldeMagickShoppe)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait

newtype UptownYeOldeMagickShoppe = UptownYeOldeMagickShoppe LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptownYeOldeMagickShoppe :: LocationCard UptownYeOldeMagickShoppe
uptownYeOldeMagickShoppe = location UptownYeOldeMagickShoppe Cards.uptownYeOldeMagickShoppe 3 (PerPlayer 2)

instance HasAbilities UptownYeOldeMagickShoppe where
  getAbilities (UptownYeOldeMagickShoppe a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> can.search.deck You) actionAbility

instance RunMessage UptownYeOldeMagickShoppe where
  runMessage msg l@(UptownYeOldeMagickShoppe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 9] (basic $ #asset <> oneOf [#spell, CardWithTrait Ritual]) (defer attrs IsNotDraw)
      pure l
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      chooseTargetM iid cards \card -> addToHand iid (only card)
      pure l
    SearchNoneFound iid (isTarget attrs -> True) -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> UptownYeOldeMagickShoppe <$> liftRunMessage msg attrs
