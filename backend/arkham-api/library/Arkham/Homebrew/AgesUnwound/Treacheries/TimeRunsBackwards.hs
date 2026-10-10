module Arkham.Homebrew.AgesUnwound.Treacheries.TimeRunsBackwards (timeRunsBackwards) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype TimeRunsBackwards = TimeRunsBackwards TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The weakness every investigator defeated in this scenario adds to their
deck; four copies are set aside at setup.
-}
timeRunsBackwards :: TreacheryCard TimeRunsBackwards
timeRunsBackwards = treachery TimeRunsBackwards Cards.timeRunsBackwards

{- | "Revelation - Shuffle your hand into your deck. If at least 3 cards are
shuffled in this way, draw a card and add a non-exceptional card from your
discard pile to your hand."

The count is of cards actually shuffled in, which is the hand as it stands when
the Revelation resolves, so it is read before the shuffle is queued.
-}
instance RunMessage TimeRunsBackwards where
  runMessage msg t@(TimeRunsBackwards attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hand <- iid.hand
      shuffleCardsIntoDeck iid hand
      when (length hand >= 3) do
        drawCards iid attrs 1
        discards <- select $ inDiscardOf iid <> basic NonExceptional
        unless (null discards) do
          focusCards discards $ chooseOneM iid $ targets discards (addToHand iid . only)
      pure t
    _ -> TimeRunsBackwards <$> liftRunMessage msg attrs
