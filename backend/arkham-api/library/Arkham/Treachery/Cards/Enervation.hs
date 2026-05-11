module Arkham.Treachery.Cards.Enervation (enervation) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (InvestigatorDamage)

newtype Enervation = Enervation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enervation :: TreacheryCard Enervation
enervation = treachery Enervation Cards.enervation

instance RunMessage Enervation where
  runMessage msg t@(Enervation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat
        $ SubtractCalculation (Fixed 5) (InvestigatorFieldCalculation iid InvestigatorDamage)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cards <- select $ inHandOf NotForPlay iid
      let highestCost = maxesBy (.printedCost) cards
      chooseOrRunOneM iid $ withI18n do
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        unless (null highestCost) do
          cardI18n
            $ scope "enervation"
            $ labeled' "discardHighestCost"
            $ chooseTargetM iid highestCost
            $ discardCard iid attrs
      pure t
    _ -> Enervation <$> liftRunMessage msg attrs
