module Arkham.Treachery.Cards.TracesToNowhere.BackToSquareOne (backToSquareOne) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BackToSquareOne = BackToSquareOne TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backToSquareOne :: TreacheryCard BackToSquareOne
backToSquareOne = treachery BackToSquareOne Cards.backToSquareOne

instance RunMessage BackToSquareOne where
  runMessage msg t@(BackToSquareOne attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      clues <- field InvestigatorClues iid
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ 1 + min 3 clues)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      spendResources iid 3
      pure t
    _ -> BackToSquareOne <$> liftRunMessage msg attrs
