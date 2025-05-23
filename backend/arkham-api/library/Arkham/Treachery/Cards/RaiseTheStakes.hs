module Arkham.Treachery.Cards.RaiseTheStakes (raiseTheStakes) where

import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectWhen)
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Placement
import Arkham.Name qualified as Name
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheHouseAlwaysWins.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RaiseTheStakes = RaiseTheStakes TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

raiseTheStakes :: TreacheryCard RaiseTheStakes
raiseTheStakes = treachery RaiseTheStakes Cards.raiseTheStakes

instance HasModifiersFor RaiseTheStakes where
  getModifiersFor (RaiseTheStakes attrs) = do
    case attrs.placement of
      InThreatArea iid -> do
        modifySelect attrs (EnemyAt $ locationWithInvestigator iid) [RemoveKeyword Aloof]
        n <- getCurrentActStep
        modifySelectWhen attrs (n >= 2) (enemyEngagedWith iid) [EnemyFight 1, EnemyEvade 1]
      _ -> pure ()

instance RunMessage RaiseTheStakes where
  runMessage msg t@(RaiseTheStakes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      name <- iid.name
      hasResources <- (> 0) <$> iid.resources
      chooseOneM iid do
        scenarioI18n $ labeled' "cheated" $ remember $ Cheated $ Name.labeled name iid
        when hasResources $ withI18n $ countVar 5 $ labeled' "loseResources" $ loseResources iid attrs 5
        scenarioI18n $ labeled "raiseTheStakes.place" $ place attrs (InThreatArea iid)
      pure t
    _ -> RaiseTheStakes <$> liftRunMessage msg attrs
