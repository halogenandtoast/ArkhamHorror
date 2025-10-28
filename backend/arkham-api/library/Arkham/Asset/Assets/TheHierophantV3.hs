module Arkham.Asset.Assets.TheHierophantV3 (theHierophantV3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Slot

newtype TheHierophantV3 = TheHierophantV3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHierophantV3 :: AssetCard TheHierophantV3
theHierophantV3 = asset TheHierophantV3 Cards.theHierophantV3

instance HasAbilities TheHierophantV3 where
  getAbilities (TheHierophantV3 a) = [restricted a 1 InYourHand $ freeReaction (GameBegins #when)]

instance HasModifiersFor TheHierophantV3 where
  getModifiersFor (TheHierophantV3 attrs) =
    controllerGets
      attrs
      [ SlotCanBe ArcaneSlot AccessorySlot
      , SlotCanBe AccessorySlot ArcaneSlot
      ]

instance RunMessage TheHierophantV3 where
  runMessage msg a@(TheHierophantV3 attrs) = runQueueT $ case msg of
    InHand iid (UseThisAbility iid' (isSource attrs -> True) 1) | iid == iid' -> do
      putCardIntoPlay iid attrs
      pure a
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid ArcaneSlot (Slot (toSource attrs) [])
      TheHierophantV3 <$> liftRunMessage msg attrs
    _ -> TheHierophantV3 <$> liftRunMessage msg attrs
