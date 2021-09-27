module Arkham.Types.Treachery.Cards.MysteriousChantingSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Mysterious Chanting" $ do
  it "will place a token on the nearest cultist" $ do
    investigator <- testInvestigator "00000" id
    cultist <- createEnemy <$> genEncounterCard Cards.acolyte
    mysteriousChanting <- genEncounterCard Cards.mysteriousChanting
    (location1, location2) <- testConnectedLocations id id
    gameTest
        investigator
        [ SetEncounterDeck (Deck [mysteriousChanting, mysteriousChanting])
        , placedLocation location1
        , placedLocation location2
        , enemySpawn location1 cultist
        , moveTo investigator location1
        , RemoveAllDoom
        , InvestigatorDrawEncounterCard (toId investigator)
        ]
        ((enemiesL %~ insertEntity cultist)
        . (locationsL %~ insertEntity location1)
        . (locationsL %~ insertEntity location2)
        )
      $ do
          runMessages
          chooseOnlyOption "choose cultist"
          getCount (toId cultist) `shouldReturn` DoomCount 2
