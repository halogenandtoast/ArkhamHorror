module Arkham.Treachery.Cards.DiabolicVoices (diabolicVoices) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DiabolicVoices = DiabolicVoices TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diabolicVoices :: TreacheryCard DiabolicVoices
diabolicVoices = treachery DiabolicVoices Cards.diabolicVoices

instance RunMessage DiabolicVoices where
  runMessage msg t@(DiabolicVoices attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 3, ScenarioInDiscardCountCalculation (cardIs Cards.diabolicVoices)]
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      handCount <- fieldMap InvestigatorHand length iid
      randomDiscardN iid attrs (min n handCount)
      doStep (n - handCount) msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1

      doStep (n - 1) msg'
      pure t
    _ -> DiabolicVoices <$> liftRunMessage msg attrs
