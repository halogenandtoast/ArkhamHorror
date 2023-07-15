module Arkham.Investigator.Cards.RolandBanksSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Types (EnemyAttrs (..))
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (LocationAttrs (..))
import Arkham.Token

spec :: Spec
spec = describe "Roland Banks" $ do
  context "ability" $ do
    it
      "after defeating an enemy, allows you to discover a clue at your location"
      $ gameTestWith Investigators.rolandBanks
      $ \rolandBanks -> do
        enemy <- testEnemy $
          \attrs -> attrs {enemyFight = 4, enemyHealth = Static 1}
        location <- testLocation $ \attrs -> attrs {locationTokens = setTokens Clue 1 mempty}
        pushAndRunAll
          [ SetChaosTokens [Zero]
          , enemySpawn location enemy
          , moveTo rolandBanks location
          , fightEnemy rolandBanks enemy
          ]
        chooseOnlyOption "start skill test"
        chooseOnlyOption "apply results"
        chooseOptionMatching
          "use ability"
          ( \case
              AbilityLabel {} -> True
              _ -> False
          )
        fieldAssert InvestigatorClues (== 1) rolandBanks

  context "elder sign" $ do
    it "gives +1 for each clue on your location" $ gameTestWith Investigators.rolandBanks $ \rolandBanks -> do
      location <- testLocation $
        \attrs -> attrs {locationTokens = setTokens Clue 1 mempty, locationShroud = 4}
      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , moveTo rolandBanks location
        , investigate rolandBanks location
        ]
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      fieldAssert InvestigatorClues (== 1) rolandBanks
