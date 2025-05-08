module Arkham.Location.Cards.Courtyard (courtyard) where

import Arkham.Ability
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Courtyard = Courtyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtyard :: LocationCard Courtyard
courtyard = location Courtyard Cards.courtyard 5 (Static 0)

instance HasAbilities Courtyard where
  getAbilities (Courtyard a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ Enters #after You ThisLocation

instance RunMessage Courtyard where
  runMessage msg l@(Courtyard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 1 attrs
      pure l
    DiscardedTopOfEncounterDeck iid [card] _ (isTarget attrs -> True) -> do
      when (toCardType card == EnemyType) $ drawCard iid card
      pure l
    _ -> Courtyard <$> liftRunMessage msg attrs
