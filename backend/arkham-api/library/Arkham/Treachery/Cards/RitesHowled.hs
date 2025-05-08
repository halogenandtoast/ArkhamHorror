module Arkham.Treachery.Cards.RitesHowled (ritesHowled) where

import Arkham.Helpers.Card (isWeakness)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RitesHowled = RitesHowled TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritesHowled :: TreacheryCard RitesHowled
ritesHowled = treachery RitesHowled Cards.ritesHowled

instance RunMessage RitesHowled where
  runMessage msg t@(RitesHowled attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      eachInvestigator \iid -> discardTopOfDeck iid attrs 3
      doStep 2 msg
      pure t
    DoStep 2 (Revelation _iid (isSource attrs -> True)) -> do
      selectEach (InvestigatorAt $ LocationWithTrait Altered) \iid -> do
        weaknesses <- filter isWeakness <$> iid.discard
        unless (null weaknesses) $ shuffleCardsIntoDeck iid weaknesses
      pure t
    _ -> RitesHowled <$> liftRunMessage msg attrs
