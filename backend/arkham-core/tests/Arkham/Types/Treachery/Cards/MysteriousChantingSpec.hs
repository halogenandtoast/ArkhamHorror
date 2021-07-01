module Arkham.Types.Treachery.Cards.MysteriousChantingSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.EncounterCard
import qualified Arkham.Types.Trait as Trait

spec :: Spec
spec = describe "Mysterious Chanting" $ do
  it "will place a token on the nearest cultist" $ do
    investigator <- testInvestigator "00000" id
    cultist <- testEnemyWithDef (cardTraitsL .~ singleton Trait.Cultist) id
    mysteriousChanting <- lookupEncounterCard "01171" <$> getRandom
    (location1, location2) <- testConnectedLocations id id
    gameTest
        investigator
        [ SetEncounterDeck [mysteriousChanting, mysteriousChanting]
        , placedLocation location1
        , placedLocation location2
        , enemySpawn location1 cultist
        , moveTo investigator location1
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
