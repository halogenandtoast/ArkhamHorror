{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arkham.Event.Cards.BackstabSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Enemy.Attrs qualified as EnemyAttrs
import Arkham.Enemy.Attrs (Field(..))
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Attrs (Field(..), InvestigatorAttrs(..))
import Arkham.Projection

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" $ do
      location <- testLocation id
      investigator <- testJenny
        $ \attrs -> attrs { investigatorCombat = 1, investigatorAgility = 4 }
      backstab <- buildEvent Events.backstab investigator
      let Just backstabCard = preview _PlayerCard (toCard $ toAttrs backstab)
      enemy <- testEnemy
        ((EnemyAttrs.fightL .~ 3) . (EnemyAttrs.healthL .~ Static 4))
      gameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator backstab
          ]
          ((entitiesL . eventsL %~ insertEntity backstab)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          )
        $ do
            runMessages
            chooseOnlyOption "Fight enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"
            assert $ fieldP EnemyDamage (== 3) (toId enemy)
            assert $ fieldP InvestigatorDiscard (== [backstabCard]) (toId investigator)
