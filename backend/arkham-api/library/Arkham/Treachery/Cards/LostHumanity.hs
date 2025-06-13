module Arkham.Treachery.Cards.LostHumanity (lostHumanity) where

import Arkham.Investigator.Projection ()
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostHumanity = LostHumanity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostHumanity :: TreacheryCard LostHumanity
lostHumanity = treachery LostHumanity Cards.lostHumanity

instance RunMessage LostHumanity where
  runMessage msg t@(LostHumanity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 5)
      doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      handCount <- length <$> iid.hand
      deckCount <- length <$> iid.deck
      discardCount <- length <$> iid.discard
      when (handCount + deckCount + discardCount < 10) $ drivenInsane iid
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      cards <- iid.topOfDeckN n
      for_ cards removeCardFromGame
      pure t
    _ -> LostHumanity <$> liftRunMessage msg attrs
