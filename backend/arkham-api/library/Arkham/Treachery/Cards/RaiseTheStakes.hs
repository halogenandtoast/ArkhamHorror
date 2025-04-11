module Arkham.Treachery.Cards.RaiseTheStakes (raiseTheStakes) where

import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Projection ()
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Placement
import Arkham.Name qualified as Name
import Arkham.Placement
import Arkham.ScenarioLogKey
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
        when (n >= 2) do
          modifySelect attrs (enemyEngagedWith iid) [EnemyFight 1, EnemyEvade 1]
      _ -> pure ()

instance RunMessage RaiseTheStakes where
  runMessage msg t@(RaiseTheStakes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      name <- iid.name
      hasResources <- (> 0) <$> iid.resources
      chooseOneM iid do
        labeled "Remember that you have \"cheated.\"" $ remember $ Cheated $ Name.labeled name iid
        when hasResources do
          labeled "Lose 5 resources" $ loseResources iid attrs 5
        labeled "Put Raise the Stakes into play in your threat area" $ place attrs (InThreatArea iid)
      pure t
    _ -> RaiseTheStakes <$> liftRunMessage msg attrs
