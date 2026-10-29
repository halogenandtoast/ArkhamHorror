module Arkham.Matcher.YetToTakeTurnSpec (spec) where

import Arkham.Ability.Types (abilityIndex)
import Arkham.Entities qualified as Entities
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Phase (Phase (EnemyPhase))
import TestImport.New

setEndedTurn :: Investigator -> Bool -> TestAppT ()
setEndedTurn investigator ended =
  overTest
    $ entitiesL
    . Entities.investigatorsL
    . ix investigator.id
    %~ overAttrs (endedTurnL .~ ended)

spec :: Spec
spec = describe "YetToTakeTurn" do
  it "excludes completed turns when nobody is taking a turn" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    setEndedTurn self True
    overTest $ turnPlayerInvestigatorIdL .~ Nothing
    select YetToTakeTurn `shouldMatchListM` [other.id]

  it "excludes the current turn even before its ended-turn flag is set" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    overTest $ turnPlayerInvestigatorIdL ?~ self.id
    select YetToTakeTurn `shouldMatchListM` [other.id]

  it "allows The Present before a turn, but not after it in the enemy phase" . gameTest $ \self -> do
    location <- testLocationWithDef Locations.thePresent id
    self `moveTo` location
    overTest $ (turnPlayerInvestigatorIdL .~ Nothing) . (phaseL .~ EnemyPhase)
    let offered = map abilityIndex <$> getActionsFrom self location
    offered `shouldSatisfyM` elem 1
    setEndedTurn self True
    offered `shouldSatisfyM` notElem 1
    setEndedTurn self False
    offered `shouldSatisfyM` elem 1
