module Arkham.Treachery.Cards.BeyondTheVeil (BeyondTheVeil (..), beyondTheVeil) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (DeckHasNoCards)

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheVeil :: TreacheryCard BeyondTheVeil
beyondTheVeil = treachery BeyondTheVeil Cards.beyondTheVeil

instance HasAbilities BeyondTheVeil where
  getAbilities (BeyondTheVeil x) = [restricted x 1 InYourThreatArea $ forced $ DeckHasNoCards #when You]

instance RunMessage BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      canAttach <- selectNone $ treacheryIs Cards.beyondTheVeil <> treacheryInThreatAreaOf iid
      when canAttach $ placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 2
      assignDamage iid source 10
      toDiscardBy iid source attrs
      pure t
    _ -> BeyondTheVeil <$> liftRunMessage msg attrs
