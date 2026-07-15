module Arkham.Homebrew.CircusExMortis.Locations.Carousel (carousel) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype Carousel = Carousel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carousel :: LocationCard Carousel
carousel =
  location Carousel Cards.carousel 3 (PerPlayer 1)

instance HasAbilities Carousel where
  getAbilities (Carousel a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> NoCluesOnThis)
      $ freeReaction (Moves #after You AnySource Anywhere (be a))

instance RunMessage Carousel where
  runMessage msg l@(Carousel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ ConnectedTo ForMovement (be attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure l
    _ -> Carousel <$> liftRunMessage msg attrs
