{- | "__Revelation__ - Test {willpower} (3). If you fail, you must either lose 2
actions or discard 2 cards from your hand at random."
-}
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.TheLoomingDead (theLoomingDead) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype TheLoomingDead = TheLoomingDead TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLoomingDead :: TreacheryCard TheLoomingDead
theLoomingDead = treachery TheLoomingDead Cards.theLoomingDead

instance RunMessage TheLoomingDead where
  runMessage msg t@(TheLoomingDead attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ scenarioI18n $ scope "theLoomingDead" do
        labeled "loseActions" $ loseActions iid attrs 2
        labeled "discardCards" $ randomDiscardN iid attrs 2
      pure t
    _ -> TheLoomingDead <$> liftRunMessage msg attrs
