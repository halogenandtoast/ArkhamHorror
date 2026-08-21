module Arkham.Treachery.Cards.ChildrenOfBlood.Hunted.OfTheNight (ofTheNight) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Hunted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OfTheNight = OfTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ofTheNight :: TreacheryCard OfTheNight
ofTheNight = treachery OfTheNight Cards.ofTheNight

instance RunMessage OfTheNight where
  runMessage msg t@(OfTheNight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      hasResources <- fieldP InvestigatorResources (> 0) iid
      hasClues <- fieldP InvestigatorClues (> 0) iid
      chooseOneM iid $ withI18n do
        countVar 3 $ labeledValidate' hasResources "loseResources" $ loseResources iid attrs 3
        countVar 2 $ labeledValidate' hasClues "loseClues" $ removeClues attrs iid 2
      pure t
    _ -> OfTheNight <$> liftRunMessage msg attrs
