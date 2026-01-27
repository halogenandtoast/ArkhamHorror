module Arkham.Event.Events.ThroughTheCracks (throughTheCracks) where

import Arkham.Calculation
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype ThroughTheCracks = ThroughTheCracks EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheCracks :: EventCard ThroughTheCracks
throughTheCracks = event ThroughTheCracks Cards.throughTheCracks

instance RunMessage ThroughTheCracks where
  runMessage msg e@(ThroughTheCracks attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid
        $ CalculatedSkillModifier #agility
        $ MaxCalculation (Fixed 3)
        $ InvestigatorFieldCalculation iid InvestigatorClues
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      enemies <- select (enemyEngagedWith iid)
      locations <-
        getCanMoveToMatchingLocations iid attrs
          $ ConnectedFrom ForMovement (locationWithInvestigator iid)
          <> #revealed

      let label =
            if
              | null locations -> "Disengage from each enemy engaged with you."
              | null enemies -> "Move to a revealed connecting location."
              | otherwise ->
                  "Disengage from each enemy engaged with you and move to a revealed connecting location."

      unless (null enemies && null locations) do
        chooseOneM iid do
          labeled label do
            for_ enemies (disengageEnemy iid)
            chooseTargetM iid locations (moveTo attrs iid)
          withI18n skip_
      pure e
    _ -> ThroughTheCracks <$> liftRunMessage msg attrs
