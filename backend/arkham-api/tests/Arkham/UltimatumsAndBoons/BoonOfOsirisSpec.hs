module Arkham.UltimatumsAndBoons.BoonOfOsirisSpec (spec) where

import Arkham.Projection (field)
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of Osiris" $ do
  it "replaces your first defeat: suffer trauma, heal everything, stay in the game" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfOsiris]
    withProp @"health" 3 self
    run $ assignDamage (toId self) (TestSource mempty) 3
    applyAllDamage
    useForcedAbility
    -- defeated by damage -> 1 physical trauma, everything healed, not defeated
    field InvestigatorPhysicalTrauma (toId self) `shouldReturn` 1
    self.mentalTrauma `shouldReturn` 0
    self.damage `shouldReturn` 0
    self.horror `shouldReturn` 0
    self.defeated `shouldReturn` False

    -- cannot be damaged until the beginning of their next turn
    run $ assignDamage (toId self) (TestSource mempty) 1
    applyAllDamage
    self.damage `shouldReturn` 0

    -- horror is not blocked
    run $ assignHorror (toId self) (TestSource mempty) 1
    applyAllHorror
    self.horror `shouldReturn` 1

    -- the immunity expires at the beginning of their next turn
    run $ BeginTurn (toId self)
    run $ assignDamage (toId self) (TestSource mempty) 1
    applyAllDamage
    self.damage `shouldReturn` 1

    -- only the FIRST defeat is replaced; a second one is real
    healedAgain <- createMessageChecker \case
      HealAllDamageAndHorror _ (UltimatumOrBoonSource (Boon BoonOfOsiris)) -> True
      _ -> False
    run $ assignDamage (toId self) (TestSource mempty) 2
    applyAllDamage
    self.defeated `shouldReturn` True
    healedAgain `refShouldBe` False
