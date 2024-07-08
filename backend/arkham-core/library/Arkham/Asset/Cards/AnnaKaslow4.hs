module Arkham.Asset.Cards.AnnaKaslow4 (annaKaslow4, AnnaKaslow4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Slot
import Arkham.Strategy

newtype AnnaKaslow4 = AnnaKaslow4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

annaKaslow4 :: AssetCard AnnaKaslow4
annaKaslow4 = ally AnnaKaslow4 Cards.annaKaslow4 (1, 1)

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) []

instance HasAbilities AnnaKaslow4 where
  getAbilities (AnnaKaslow4 a) =
    [ restrictedAbility a 1 InYourHand $ freeReaction $ GameBegins #when
    , controlledAbility a 2 (CanSearchDeck <> CanShuffleDeck)
        $ freeReaction
        $ AssetEntersPlay #when (be a)
    ]

instance RunMessage AnnaKaslow4 where
  runMessage msg a@(AnnaKaslow4 attrs) = runQueueT $ case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      putCardIntoPlay iid attrs
      pure a
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid TarotSlot (slot attrs))
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      search iid (attrs.ability 2) iid [fromDeck] (basic $ #tarot <> #asset) $ PlayFound iid 1
      pure a
    _ -> AnnaKaslow4 <$> liftRunMessage msg attrs
