module Arkham.Treachery.Cards.Trespasser (trespasser) where

import Arkham.Calculation
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Trespasser = Trespasser TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trespasser :: TreacheryCard Trespasser
trespasser = treachery Trespasser Cards.trespasser

instance RunMessage Trespasser where
  runMessage msg t@(Trespasser attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation
          [ Fixed 2
          , CountTreacheries $ TreacheryInThreatAreaOf (InvestigatorWithId iid) <> TreacheryIsNonWeakness
          ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        countVar 2 $ labeled' "takeHorror" $ assignDamage iid attrs 2
      pure t
    _ -> Trespasser <$> liftRunMessage msg attrs
