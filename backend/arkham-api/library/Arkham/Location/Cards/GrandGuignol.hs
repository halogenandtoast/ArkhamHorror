module Arkham.Location.Cards.GrandGuignol (grandGuignol) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrandGuignol = GrandGuignol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandGuignol :: LocationCard GrandGuignol
grandGuignol = location GrandGuignol Cards.grandGuignol 3 (PerPlayer 1)

instance HasAbilities GrandGuignol where
  getAbilities (GrandGuignol a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ RevealLocation #after You (be a)

instance RunMessage GrandGuignol where
  runMessage msg a@(GrandGuignol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nonWeaknessCards <- select $ basic NonWeakness <> inHandOf NotForPlay iid
      chooseOrRunOneM iid do
        labeled "Take 2 Horror" $ assignHorror iid (attrs.ability 1) 2
        unless (null nonWeaknessCards) do
          labeled
            "Shuffle all non-weakness cards from your hand into your deck, then draw an equal number of cards"
            do
              shuffleCardsIntoDeck iid nonWeaknessCards
              drawCards iid (attrs.ability 1) (length nonWeaknessCards)
      pure a
    _ -> GrandGuignol <$> liftRunMessage msg attrs
