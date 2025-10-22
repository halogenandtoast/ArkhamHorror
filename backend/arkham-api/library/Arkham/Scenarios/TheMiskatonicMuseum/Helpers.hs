module Arkham.Scenarios.TheMiskatonicMuseum.Helpers where

import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.I18n
import Arkham.Id
import Arkham.Layout
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theMiskatonicMuseum" a

getInPlayHuntingHorror :: (Tracing m, HasGame m) => m (Maybe EnemyId)
getInPlayHuntingHorror = getHuntingHorrorWith AnyInPlayEnemy

getHuntingHorror :: (Tracing m, HasGame m) => m (Maybe EnemyId)
getHuntingHorror = getHuntingHorrorWith AnyEnemy

getHuntingHorrorWith :: (Tracing m, HasGame m) => EnemyMatcher -> m (Maybe EnemyId)
getHuntingHorrorWith matcher = selectOne $ enemyIs Cards.huntingHorror <> matcher

getRestrictedHall :: (Tracing m, HasGame m) => m LocationId
getRestrictedHall = selectJust $ LocationWithFullTitle "Exhibit Hall" "Restricted Hall"

scenarioLayout :: [GridTemplateRow]
scenarioLayout =
  [ ".     .     .                    .                    hall3 hall3          hall4          hall4 .                  .              .     ."
  , ".     .     hall2                hall2                hall3 hall3          hall4          hall4 hall5              hall5          .     ."
  , "hall1 hall1 hall2                hall2                .     museumHalls    museumHalls    .     hall5              hall5          hall6 hall6"
  , "hall1 hall1 .                    .                    .     museumHalls    museumHalls    .     .                  .              hall6 hall6"
  , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
  , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
  ]
