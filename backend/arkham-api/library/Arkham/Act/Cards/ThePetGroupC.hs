module Arkham.Act.Cards.ThePetGroupC (thePetGroupC) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Scenario (inVictoryDisplay)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype ThePetGroupC = ThePetGroupC ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Timed - Do not advance this act until you are instructed. Find a way to
-- deal with Eixodolon's Pet before the agenda advances.
thePetGroupC :: ActCard ThePetGroupC
thePetGroupC = act (2, A) ThePetGroupC Cards.thePetGroupC Nothing

instance RunMessage ThePetGroupC where
  runMessage msg a@(ThePetGroupC attrs) = runQueueT $ scenarioI18n $ scope "thePet" $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      defeated <- inVictoryDisplay $ cardIs Enemies.eixodolonsPet
      if defeated
        then flavor $ h "title" >> p "victory"
        else do
          mPet <- selectOne $ enemyIs Enemies.eixodolonsPet
          for_ mPet \pet -> do
            flavor $ h "title" >> p "breaksFree"
            hunger <- selectJust $ locationIs Locations.chamberOfHunger
            place pet (AtLocation hunger)
            investigators <- select $ InvestigatorAt $ locationIs Locations.chamberOfHunger
            for_ investigators \iid -> initiateEnemyAttack pet attrs iid
      push $ ScenarioSpecific "act3Setup" Null
      advanceActDeck attrs
      pure a
    _ -> ThePetGroupC <$> liftRunMessage msg attrs
