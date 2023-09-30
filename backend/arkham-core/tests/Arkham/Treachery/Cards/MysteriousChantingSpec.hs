module Arkham.Treachery.Cards.MysteriousChantingSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher (defaultRemoveDoomMatchers)
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Mysterious Chanting" $ do
  it "will place a token on the nearest cultist" $ gameTest $ \investigator -> do
    cultist <- testEnemyWithDef Cards.acolyte id
    mysteriousChanting <- genEncounterCard Cards.mysteriousChanting
    (location1, location2) <- testConnectedLocations id id
    pushAndRunAll
      [ SetEncounterDeck (Deck [mysteriousChanting, mysteriousChanting])
      , placedLocation location1
      , placedLocation location2
      , spawnAt cultist location1
      , moveTo investigator location1
      , RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      , InvestigatorDrawEncounterCard (toId investigator)
      ]
    chooseOnlyOption "choose cultist"
    fieldAssert EnemyDoom (== 2) cultist
