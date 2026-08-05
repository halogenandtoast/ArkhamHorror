module Arkham.Location.Cards.SouthsideRuined (southsideRuined) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Item))

newtype SouthsideRuined = SouthsideRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideRuined :: LocationCard SouthsideRuined
southsideRuined = location SouthsideRuined Cards.southsideRuined 3 (Static 1)

itemInDiscard :: ExtendedCardMatcher
itemInDiscard = InDiscardOf You <> basic (#asset <> CardWithTrait Item)

instance HasAbilities SouthsideRuined where
  getAbilities (SouthsideRuined a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists itemInDiscard) actionAbility

instance RunMessage SouthsideRuined where
  runMessage msg l@(SouthsideRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select itemInDiscard
      focusCards cards do
        chooseOneM iid $ targets cards $ addToHand iid . only
        unfocusCards
      pure l
    _ -> SouthsideRuined <$> liftRunMessage msg attrs
