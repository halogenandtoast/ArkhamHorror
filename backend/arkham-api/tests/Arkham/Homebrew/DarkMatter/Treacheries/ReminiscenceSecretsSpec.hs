module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscenceSecretsSpec (spec) where

import Arkham.Helpers.Message qualified as Helpers
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Placement (Placement (HiddenInHand))
import TestImport.New

spec :: Spec
spec = describe "Reminiscence (Secrets)" do
  -- "place 1 of their clues on that location instead of discovering clues" —
  -- without popping the queued discovery they would discover *and* place.
  it "places a clue instead of discovering" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    withProp @"clues" 1 self
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
    secrets <- genCard Treacheries.reminiscenceSecrets
    createSecrets <- Helpers.createTreacheryAt_ secrets (HiddenInHand self.id)
    run createSecrets
    setChaosTokens [Zero]
    self `moveTo` location
    self `investigate` location
    startSkillTest
    applyResults
    useReaction
    self.clues `shouldReturn` 0
    location.clues `shouldReturn` 2

  -- Placing the clue is the cost of the replacement, so it can't be triggered
  -- by an investigator with no clue to place.
  it "is not offered when that investigator has no clues" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    withProp @"clues" 0 self
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
    secrets <- genCard Treacheries.reminiscenceSecrets
    createSecrets <- Helpers.createTreacheryAt_ secrets (HiddenInHand self.id)
    run createSecrets
    setChaosTokens [Zero]
    self `moveTo` location
    self `investigate` location
    startSkillTest
    applyResults
    assertHasNoReaction
    self.clues `shouldReturn` 1
    location.clues `shouldReturn` 0
