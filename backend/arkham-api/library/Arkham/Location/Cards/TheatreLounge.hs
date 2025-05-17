module Arkham.Location.Cards.TheatreLounge (theatreLounge) where

import Arkham.Ability
import Arkham.Card
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype TheatreLounge = TheatreLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theatreLounge :: LocationCard TheatreLounge
theatreLounge = location TheatreLounge Cards.theatreLounge 2 (PerPlayer 1)

instance HasAbilities TheatreLounge where
  getAbilities (TheatreLounge a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage TheatreLounge where
  runMessage msg l@(TheatreLounge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleEncounterDiscardBackIn
      search iid (attrs.ability 1) EncounterDeckTarget [fromDeck] #any (defer attrs IsNotDraw)
      pure l
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      addToHand iid $ take 2 $ filterCards (CardWithKeyword Keyword.Hidden) cards
      pure l
    _ -> TheatreLounge <$> liftRunMessage msg attrs
