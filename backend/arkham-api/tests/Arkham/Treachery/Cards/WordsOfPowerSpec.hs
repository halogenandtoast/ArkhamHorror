module Arkham.Treachery.Cards.WordsOfPowerSpec (spec) where

import Arkham.Constants (pattern AbilityAttack)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Words of Power" $ do
  -- Regression for issue #4887: "you cannot damage those enemies" must only
  -- block the owner of Words of Power, not other investigators. A basic
  -- attack's damage source is AbilitySource (EnemySource <enemy>) AbilityAttack
  -- (whose underlying source is the encounter enemy); InvestigatorDamageEnemy
  -- with that source is exactly what the fight cascade pushes on a successful
  -- fight (see Arkham.Behavior.Fight). The owner is identified by the
  -- performer recorded in the source, not by it being an encounter card.
  it "blocks only the owner from damaging doomed enemies at their location (#4887)"
    . gameTestWith Investigators.carsonSinclair
    $ \carson -> do
      silas <- addInvestigator Investigators.silasMarsh
      enemy <- testEnemy & prop @"health" 5
      location <- testLocation
      enemy `spawnAt` location
      carson `moveTo` location
      silas `moveTo` location

      -- Put Words of Power into Carson's threat area and a doom on the enemy.
      -- It is an encounter treachery, so draw it from the encounter deck; its
      -- Revelation places it in the drawing investigator's threat area.
      wordsOfPower <- genEncounterCard Cards.wordsOfPower
      run $ SetEncounterDeck (Deck [wordsOfPower])
      run $ drawEncounterCard carson.id GameSource
      run $ PlaceDoom GameSource (toTarget enemy) 1
      assert
        $ selectAny
        $ TreacheryInThreatAreaOf (InvestigatorWithId carson.id)
        <> treacheryIs Cards.wordsOfPower

      -- Carson (the owner) cannot damage the doomed enemy.
      run $ InvestigatorDamageEnemy carson.id enemy.id (toAbilitySource enemy AbilityAttack)
      enemy.damage `shouldReturn` 0

      -- Another investigator at the same location can.
      run $ InvestigatorDamageEnemy silas.id enemy.id (toAbilitySource enemy AbilityAttack)
      enemy.damage `shouldReturn` 1
