module Arkham.Homebrew.CircusExMortis.Treacheries.QuickerThanTheEye (quickerThanTheEye) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Performer))
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype QuickerThanTheEye = QuickerThanTheEye TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickerThanTheEye :: TreacheryCard QuickerThanTheEye
quickerThanTheEye = treachery QuickerThanTheEye Cards.quickerThanTheEye

instance RunMessage QuickerThanTheEye where
  runMessage msg t@(QuickerThanTheEye attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      hasPerformer <- selectAny $ enemyAtLocationWith iid <> EnemyWithTrait Performer
      revelationSkillTest sid iid attrs #willpower (Fixed $ if hasPerformer then 4 else 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        chooseTakeHorror iid attrs 2
        countVar 2 $ labeled' "discardCards" $ chooseAndDiscardCards iid attrs 2
      pure t
    _ -> QuickerThanTheEye <$> liftRunMessage msg attrs
