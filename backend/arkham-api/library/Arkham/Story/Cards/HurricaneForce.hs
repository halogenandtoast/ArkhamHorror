module Arkham.Story.Cards.HurricaneForce (hurricaneForce) where

import Arkham.Campaigns.TheDrownedCity.Helpers (increaseFloodLevel)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Helpers.Query (getLead)
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
      rage <- getCthulhuRage

      -- "If Cthulhu (Hoary Wings) is in play: Resolve Cthulhu's patrol keyword an
      -- additional time."
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuHoaryWings) resolveCthulhuPatrol

      {- "If Cthulhu (Fierce Visage) is in play: The nearest investigator to Cthulhu
      must test [willpower] (X), where X is Cthulhu's Rage. If that investigator
      fails, Cthulhu (Fierce Visage) attacks them." -}
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuFierceVisage) do
        selectEach (NearestToEnemy $ enemyIs Enemies.cthulhuAncientEvil) \iid -> do
          sid <- getRandom
          onFailedByEffect sid AnyValue attrs iid $ void $ cthulhuFacetAttacks attrs Enemies.cthulhuFierceVisage iid
          beginSkillTest sid iid attrs iid #willpower (Fixed rage)

      {- "If Cthulhu (Wicked Claw) is in play: Choose an investigator at Cthulhu's
      location to test [agility] (X), where X is Cthulhu's Rage. If that
      investigator fails, they must either increase the flood level of Cthulhu's
      location, or take 1 damage or 1 horror." -}
      whenM (selectAny $ cthulhuFacet Enemies.cthulhuWickedClaw) do
        getCthulhuLocation >>= traverse_ \lid -> do
          lead <- getLead
          investigators <- select $ investigatorAt lid
          chooseOrRunOneM lead $ scenarioI18n do
            questionLabeled' "chooseInvestigatorToTest"
            targets investigators \iid -> do
              sid <- getRandom
              onFailedByEffect sid AnyValue attrs iid $ chooseOneM iid do
                scenarioI18n $ labeled' "increaseFloodLevel" $ increaseFloodLevel lid
                sharedI18n $ countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
                sharedI18n $ countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
              beginSkillTest sid iid attrs iid #agility (Fixed rage)
      pure s
    _ -> HurricaneForce <$> liftRunMessage msg attrs
