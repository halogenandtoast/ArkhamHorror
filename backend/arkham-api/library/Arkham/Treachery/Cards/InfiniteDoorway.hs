module Arkham.Treachery.Cards.InfiniteDoorway (
  infiniteDoorway,
  InfiniteDoorway (..),
)
where

import Arkham.Card
import Arkham.Cost
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InfiniteDoorway = InfiniteDoorway TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor InfiniteDoorway where
  getModifiersFor target (InfiniteDoorway attrs) | treacheryOn attrs target = do
    modified
      attrs
      [ AdditionalCostToLeave $ DiscardTopOfDeckCost 1 (Just $ toTarget attrs)
      , AdditionalCostToEnter $ DiscardTopOfDeckCost 1 (Just $ toTarget attrs)
      ]
  getModifiersFor _ _ = pure []

infiniteDoorway :: TreacheryCard InfiniteDoorway
infiniteDoorway = treachery InfiniteDoorway Cards.infiniteDoorway

instance RunMessage InfiniteDoorway where
  runMessage msg t@(InfiniteDoorway attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ place attrs
      pure t
    DiscardedTopOfDeck iid cards _ (isTarget attrs -> True) -> do
      for_ cards \card -> do
        if card `cardMatch` WeaknessCard
          then drawCard iid card
          else do
            hand <- filterCards (CardWithTitle card.title) <$> iid.hand
            inPlay <- select $ inPlayAreaOf iid <> basic (CardWithTitle card.title)

            for_ (hand <> inPlay) \c -> do
              focusCards [c] \unfocus -> do
                chooseOneM iid do
                  labeled "Discard" $ discardCard iid attrs c
                  labeled "Take 1 horror" $ assignHorror iid attrs 1
                push unfocus
      pure t
    _ -> InfiniteDoorway <$> liftRunMessage msg attrs
