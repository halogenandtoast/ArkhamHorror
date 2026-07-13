module Arkham.Treachery.Cards.QuickerThanTheEyeCircusExMortis (quickerThanTheEyeCircusExMortis) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Performer))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype QuickerThanTheEyeCircusExMortis = QuickerThanTheEyeCircusExMortis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickerThanTheEyeCircusExMortis :: TreacheryCard QuickerThanTheEyeCircusExMortis
quickerThanTheEyeCircusExMortis = treachery QuickerThanTheEyeCircusExMortis Cards.quickerThanTheEyeCircusExMortis

instance RunMessage QuickerThanTheEyeCircusExMortis where
  runMessage msg t@(QuickerThanTheEyeCircusExMortis attrs) = runQueueT $ case msg of
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
    _ -> QuickerThanTheEyeCircusExMortis <$> liftRunMessage msg attrs
