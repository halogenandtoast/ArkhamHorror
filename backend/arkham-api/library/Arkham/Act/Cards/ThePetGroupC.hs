module Arkham.Act.Cards.ThePetGroupC (thePetGroupC) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Scenario (inVictoryDisplay)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype ThePetGroupC = ThePetGroupC ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePetGroupC :: ActCard ThePetGroupC
thePetGroupC = act (2, A) ThePetGroupC Cards.thePetGroupC Nothing

instance RunMessage ThePetGroupC where
  runMessage msg a@(ThePetGroupC attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      defeated <- inVictoryDisplay $ cardIs Enemies.eixodolonsPet
      unless defeated do
        withMatch (enemyIs Enemies.eixodolonsPet) \pet -> do
          hunger <- selectJust $ locationIs Locations.chamberOfHunger
          place pet (AtLocation hunger)
          investigators <- select $ InvestigatorAt $ locationIs Locations.chamberOfHunger
          for_ investigators $ initiateEnemyAttack pet attrs
      scenarioSpecific_ "act3Setup"
      advanceActDeck attrs
      pure a
    _ -> ThePetGroupC <$> liftRunMessage msg attrs
