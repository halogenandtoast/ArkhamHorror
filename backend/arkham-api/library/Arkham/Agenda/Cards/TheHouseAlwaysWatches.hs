module Arkham.Agenda.Cards.TheHouseAlwaysWatches (theHouseAlwaysWatches) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Story.Types (Field (StoryTokens))
import Arkham.Token
import Arkham.Trait (Trait (Casino))

newtype TheHouseAlwaysWatches = TheHouseAlwaysWatches AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseAlwaysWatches :: AgendaCard TheHouseAlwaysWatches
theHouseAlwaysWatches = agenda (1, A) TheHouseAlwaysWatches Cards.theHouseAlwaysWatches (Static 8)

instance HasAbilities TheHouseAlwaysWatches where
  getAbilities (TheHouseAlwaysWatches a) =
    [ restricted a 1 (exists $ InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
        $ forced
        $ PhaseBegins #when #enemy
    , mkAbility a 2 $ forced $ EnemyDefeated #after You ByAny (EnemyWithTrait Casino)
    ]

instance RunMessage TheHouseAlwaysWatches where
  runMessage msg a@(TheHouseAlwaysWatches attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1)
        =<< select (InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      raiseAlarmLevel (attrs.ability 2) [iid]
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      resources <- selectSum InvestigatorResources Anyone
      eliminatedResources <-
        fieldMap StoryTokens (countTokens #resource) =<< selectJust (storyIs Stories.theStakeout)
      n <- perPlayer 10
      when (resources + eliminatedResources >= n) $ remember CleanedOutTheHouse
      push R1
      pure a
    _ -> TheHouseAlwaysWatches <$> liftRunMessage msg attrs
