module Arkham.Treachery.Cards.BeyondTheVeil (BeyondTheVeil (..), beyondTheVeil) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (treacheryInThreatAreaOf)

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

beyondTheVeil :: TreacheryCard BeyondTheVeil
beyondTheVeil = treachery BeyondTheVeil Cards.beyondTheVeil

instance HasAbilities BeyondTheVeil where
  getAbilities (BeyondTheVeil x) = [restricted x 1 InYourThreatArea $ forced $ DeckHasNoCards #when You]

instance RunMessage BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      canAttach <- selectNone $ treacheryIs Cards.beyondTheVeil <> treacheryInThreatAreaOf iid
      pushWhen canAttach $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 2
      pushAll [assignDamage iid source 10, toDiscardBy iid source attrs]
      pure t
    _ -> BeyondTheVeil <$> runMessage msg attrs
