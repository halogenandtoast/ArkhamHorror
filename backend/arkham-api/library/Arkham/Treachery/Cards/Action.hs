module Arkham.Treachery.Cards.Action (action) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Action = Action TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

action :: TreacheryCard Action
action = treachery Action Cards.action

instance RunMessage Action where
  runMessage msg t@(Action attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasRemainingActions <- fieldP InvestigatorRemainingActions (> 0) iid
      chooseOneM iid do
        withI18n $ countVar 2 $ labeledValidate' hasRemainingActions "loseActions" $ loseActions iid attrs 2
        scenarioI18n $ labeled' "action.option" $ do_ msg
      pure t
    Do (Revelation iid (isSource attrs -> True)) -> do
      selectEach (colocatedWith iid) \iid' -> assignDamageAndHorror iid' attrs 1 1
      pure t
    _ -> Action <$> liftRunMessage msg attrs
