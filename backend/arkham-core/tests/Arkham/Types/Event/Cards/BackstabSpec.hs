module Arkham.Types.Event.Cards.BackstabSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Difficulty
import Arkham.Types.Helpers
import Arkham.Types.Token

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" $ do
      theGathering <- newScenario Easy "01104"
      (locationId, study) <- newLocation "01111"
      (investigatorId, wendyAdams) <- newInvestigator "01005"
      -- Wendy adams has combat 1 and agility 4
      (eventId, backstab) <- newEvent "01051" investigatorId
      (enemyId, ravenousGhoul) <- newEnemy "01161"
      -- Ravenous Ghoul has 3 health
      game <-
        runGameTest
          wendyAdams
          [ EnemySpawn locationId enemyId
          , MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId eventId
          ]
          ((events %~ insertMap eventId backstab)
          . (locations %~ insertMap locationId study)
          . (enemies %~ insertMap enemyId ravenousGhoul)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
          )
        >>= runGameTestOnlyOption "Fight enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      -- We expect the skill check to succeed and the enemy to be defeated
      ravenousGhoul `shouldSatisfy` isInEncounterDiscard game
      backstab `shouldSatisfy` isInDiscardOf game wendyAdams

isInDiscardOf
  :: (ToPlayerCard entity) => Game queue -> Investigator -> entity -> Bool
isInDiscardOf game investigator entity = card `elem` discard'
 where
  discard' = game ^?! investigators . ix (getId () investigator) . to discardOf
  card = asPlayerCard entity

class ToPlayerCard a where
  asPlayerCard :: a -> PlayerCard

class ToEncounterCard a where
  asEncounterCard :: a -> EncounterCard

instance ToPlayerCard Event where
  asPlayerCard event =
    lookupPlayerCard (getCardCode event) (CardId . unEventId $ getId () event)

isInEncounterDiscard :: (ToEncounterCard entity) => Game queue -> entity -> Bool
isInEncounterDiscard game entity = card `elem` discard'
 where
  discard' = game ^. discard
  card = asEncounterCard entity

instance ToEncounterCard Enemy where
  asEncounterCard enemy = lookupEncounterCard
    (getCardCode enemy)
    (CardId . unEnemyId $ getId () enemy)

