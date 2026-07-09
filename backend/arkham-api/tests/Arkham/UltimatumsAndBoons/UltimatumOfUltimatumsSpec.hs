module Arkham.UltimatumsAndBoons.UltimatumOfUltimatumsSpec (spec) where

import Arkham.Game.Settings
import Helpers.UltimatumsAndBoons
import TestImport.New

-- NOTE: the roll itself happens inside the StartScenario handler
-- (Game/Runner), which rebuilds the scenario from scratch and can't be driven
-- by the harness; these specs cover the roll pool and the plumbing that makes
-- a rolled entry take effect.
spec :: Spec
spec = describe "Ultimatum of Ultimatums" $ do
  it "excludes deckbuilding/chaos-bag entries (and itself) from the roll pool" $ do
    ( filter affectsDeckbuildingOrChaosBag allUltimatumsAndBoons
        `shouldMatchList` map Boon [BoonOfTheMorrigan, BoonOfTheAncients]
        <> map
          Ultimatum
          [ UltimatumOfBrokenPromises
          , UltimatumOfChaos
          , UltimatumOfDisaster
          , UltimatumOfFailure
          , UltimatumOfTheHighlander
          , UltimatumOfInduction
          , UltimatumOfOrthodoxy
          , UltimatumOfExile
          , UltimatumOfUltimatums
          ]
        :: IO ()
      )

  it "the rolled entry counts as active" $ do
    let rolled = Ultimatum UltimatumOfDread
    let
      settings =
        defaultSettings
          { settingsUltimatumsAndBoons = singletonSet (Ultimatum UltimatumOfUltimatums)
          , settingsRolledUltimatumOrBoon = Just rolled
          }
    ( activeUltimatumsAndBoons settings
        `shouldBe` setFromList [Ultimatum UltimatumOfUltimatums, rolled]
        :: IO ()
      )

  it "the rolled entry is inert while ultimatums and boons are disabled" $ do
    let
      settings =
        defaultSettings
          { settingsUltimatumsAndBoons = singletonSet (Ultimatum UltimatumOfUltimatums)
          , settingsRolledUltimatumOrBoon = Just (Ultimatum UltimatumOfDread)
          , settingsUltimatumsAndBoonsEnabled = False
          }
    (activeUltimatumsAndBoons settings `shouldBe` mempty :: IO ())

  it "a rolled ultimatum's effects apply to the current game" . gameTest $ \self -> do
    setUltimatumsAndBoons \s ->
      s {settingsRolledUltimatumOrBoon = Just (Ultimatum UltimatumOfHardship)}
    run $ TakeStartingResources (toId self)
    self.resources `shouldReturn` 3
