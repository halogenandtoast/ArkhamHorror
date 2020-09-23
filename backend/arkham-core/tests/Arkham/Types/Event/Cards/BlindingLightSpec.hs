module Arkham.Types.Event.Cards.BlindingLightSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Difficulty
import Arkham.Types.Helpers
import Arkham.Types.Token

spec :: Spec
spec = do
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ do
      theGathering <- newScenario Easy "01104"
      (agnesBakerId, agnesBaker) <- newInvestigator "01004"
      (icyGhoulId, icyGhoul) <- newEnemy "01119"
      (blindingLightId, blindingLight) <- newEvent "01066" agnesBakerId
      (hallwayId, hallway) <- newLocation "01112"
      game <-
        runGameTest
          agnesBaker
          [ EnemySpawn hallwayId icyGhoulId
          , MoveTo agnesBakerId hallwayId
          , InvestigatorPlayEvent agnesBakerId blindingLightId
          ]
          ((events %~ insertMap blindingLightId blindingLight)
          . (enemies %~ insertMap icyGhoulId icyGhoul)
          . (locations %~ insertMap hallwayId hallway)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight `shouldSatisfy` isInDiscardOf game agnesBaker
      icyGhoul `shouldSatisfy` evadedBy game agnesBaker

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          theDevourerBelow <- newScenario Easy "01142"
          (agnesBakerId, agnesBaker) <- newInvestigator "01004"
          (icyGhoulId, icyGhoul) <- newEnemy "01119"
          (blindingLightId, blindingLight) <- newEvent "01066" agnesBakerId
          (hallwayId, hallway) <- newLocation "01112"
          game <-
            runGameTest
              agnesBaker
              [ EnemySpawn hallwayId icyGhoulId
              , MoveTo agnesBakerId hallwayId
              , InvestigatorPlayEvent agnesBakerId blindingLightId
              ]
              ((events %~ insertMap blindingLightId blindingLight)
              . (enemies %~ insertMap icyGhoulId icyGhoul)
              . (locations %~ insertMap hallwayId hallway)
              . (chaosBag .~ Bag [token])
              . (scenario ?~ theDevourerBelow)
              )
            >>= runGameTestOnlyOption "Evade enemy"
            >>= runGameTestOnlyOption "Run skill check"
            >>= runGameTestOnlyOption "Apply results"
            >>= (\game -> if token == Tablet
                  then runGameTestOnlyOption "take damage" game
                  else pure game
                )
          blindingLight `shouldSatisfy` isInDiscardOf game agnesBaker
          agnesBaker `shouldSatisfy` hasRemainingActions game 2

evadedBy :: Game queue -> Investigator -> Enemy -> Bool
evadedBy game _investigator enemy =
  let enemy' = game ^?! enemies . ix (getId () enemy)
  in not (isEngaged enemy') && isExhausted enemy'

hasRemainingActions :: Game queue -> Int -> Investigator -> Bool
hasRemainingActions game n investigator =
  let investigator' = game ^?! investigators . ix (getId () investigator)
  in actionsRemaining investigator' == n
