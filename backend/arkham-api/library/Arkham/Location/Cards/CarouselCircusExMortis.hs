module Arkham.Location.Cards.CarouselCircusExMortis (carouselCircusExMortis) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype CarouselCircusExMortis = CarouselCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carouselCircusExMortis :: LocationCard CarouselCircusExMortis
carouselCircusExMortis =
  location CarouselCircusExMortis Cards.carouselCircusExMortis 3 (PerPlayer 1)

instance HasAbilities CarouselCircusExMortis where
  getAbilities (CarouselCircusExMortis a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> NoCluesOnThis)
      $ freeReaction (Moves #after You AnySource Anywhere (be a))

instance RunMessage CarouselCircusExMortis where
  runMessage msg l@(CarouselCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ ConnectedTo ForMovement (be attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure l
    _ -> CarouselCircusExMortis <$> liftRunMessage msg attrs
