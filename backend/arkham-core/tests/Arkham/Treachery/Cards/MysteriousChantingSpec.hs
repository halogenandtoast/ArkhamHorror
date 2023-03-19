module Arkham.Treachery.Cards.MysteriousChantingSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Matcher (defaultRemoveDoomMatchers)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Enemy.Types (Field(..))

spec :: Spec
spec = describe "Mysterious Chanting" $ do
  it "will place a token on the nearest cultist" $ do
    investigator <- testJenny id
    cultist <- createEnemy <$> genCard Cards.acolyte <*> getRandom
    mysteriousChanting <- genEncounterCard Cards.mysteriousChanting
    (location1, location2) <- testConnectedLocations id id
    gameTest
        investigator
        [ SetEncounterDeck (Deck [mysteriousChanting, mysteriousChanting])
        , placedLocation location1
        , placedLocation location2
        , enemySpawn location1 cultist
        , moveTo investigator location1
        , RemoveAllDoomFromPlay defaultRemoveDoomMatchers
        , InvestigatorDrawEncounterCard (toId investigator)
        ]
        ((entitiesL . enemiesL %~ insertEntity cultist)
        . (entitiesL . locationsL %~ insertEntity location1)
        . (entitiesL . locationsL %~ insertEntity location2)
        )
      $ do
          runMessages
          chooseOnlyOption "choose cultist"
          fieldAssert EnemyDoom (== 2) cultist
