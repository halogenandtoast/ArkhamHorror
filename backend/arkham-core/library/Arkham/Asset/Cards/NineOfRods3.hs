module Arkham.Asset.Cards.NineOfRods3 (nineOfRods3, NineOfRods3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher

newtype NineOfRods3 = NineOfRods3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nineOfRods3 :: AssetCard NineOfRods3
nineOfRods3 = asset NineOfRods3 Cards.nineOfRods3

instance HasAbilities NineOfRods3 where
  getAbilities (NineOfRods3 a) =
    [ reactionAbility
        a
        1
        (exhaust a)
        (DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck)
        ControlsThis
    , restrictedAbility a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

instance RunMessage NineOfRods3 where
  runMessage msg a@(NineOfRods3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelCardDraw (attrs.ability 1) card
      push $ ShuffleCardsIntoDeck Deck.EncounterDeck [card]
      drawEncounterCard iid attrs
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> NineOfRods3 <$> liftRunMessage msg attrs
