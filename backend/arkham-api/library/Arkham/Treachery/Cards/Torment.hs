module Arkham.Treachery.Cards.Torment (torment) where

import Arkham.Card
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Torment = Torment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torment :: TreacheryCard Torment
torment = treachery Torment Cards.torment

instance RunMessage Torment where
  runMessage msg t@(Torment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      let choice kind = do
            eachInvestigator \iid' -> do
              chooseAndDiscardCardEdit iid' attrs \d -> d {discardFilter = CardWithType kind}
      chooseOneM iid $ withI18n do
        labeled' "assets" $ choice AssetType
        labeled' "events" $ choice EventType
        labeled' "skills" $ choice SkillType
      pure t
    _ -> Torment <$> liftRunMessage msg attrs
