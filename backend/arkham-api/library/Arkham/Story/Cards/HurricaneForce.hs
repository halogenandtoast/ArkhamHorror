module Arkham.Story.Cards.HurricaneForce (hurricaneForce) where

import Arkham.Campaigns.TheDrownedCity.Helpers (increaseFloodLevel)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HurricaneForce = HurricaneForce StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hurricaneForce :: StoryCard HurricaneForce
hurricaneForce = story HurricaneForce Cards.hurricaneForce

instance RunMessage HurricaneForce where
  runMessage msg s@(HurricaneForce attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      doStep 1 msg
      doStep 2 msg
      doStep 3 msg
      pure s
    DoStep 1 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas HoaryWings resolveCthulhuPatrol
      pure s
    DoStep 2 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas FierceVisage do
        investigators <- select $ NearestToEnemy $ enemyIs Enemies.cthulhuAncientEvil
        sid <- getRandom
        leadChooseOrRunOneM do
          targets investigators \iid ->
            beginSkillTest sid iid (indexed 1 attrs) iid #willpower (ScenarioCount CthulhuRage)
      pure s
    FailedThisSkillTest iid (isIndexedSource 1 attrs -> True) -> do
      void $ cthulhuFacetAttacks attrs FierceVisage iid
      pure s
    DoStep 3 (ResolveThisStory _ (is attrs -> True)) -> do
      whenCthulhuHas WickedClaw do
        investigators <- getInvestigatorsWithCthulhu
        sid <- getRandom
        leadChooseOrRunOneM $ scenarioI18n do
          questionLabeled' "chooseInvestigatorToTest"
          targets investigators \iid ->
            beginSkillTest sid iid (indexed 3 attrs) iid #agility (ScenarioCount CthulhuRage)
      pure s
    FailedThisSkillTest iid (isIndexedSource 3 attrs -> True) -> do
      mlid <- getCthulhuLocation
      canFlood <- maybe (pure False) (`matches` CanHaveFloodLevelIncreased) mlid
      chooseOneM iid do
        scenarioI18n $ labeledValidate' canFlood "increaseFloodLevel" $ for_ mlid increaseFloodLevel
        sharedI18n $ countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
        sharedI18n $ countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
      pure s
    _ -> HurricaneForce <$> liftRunMessage msg attrs
