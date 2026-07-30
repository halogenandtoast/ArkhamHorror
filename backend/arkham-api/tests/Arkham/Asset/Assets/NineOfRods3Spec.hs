module Arkham.Asset.Assets.NineOfRods3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Nine of Rods (3)" $ do
  -- Nine of Rods (3) reads "When you draw a non-weakness ENCOUNTER CARD", but the
  -- trigger window matched NonWeaknessTreachery, so drawing an encounter enemy
  -- offered nothing at all (found while fixing #5299).
  it "offers its reaction for a drawn non-weakness encounter enemy" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    nineOfRods <- self `putAssetIntoPlay` Assets.nineOfRods3
    swarmOfRats <- genEncounterCard Enemies.swarmOfRats
    run $ SetEncounterDeck (Deck [swarmOfRats])
    run $ drawEncounterCard self.id GameSource

    -- pre-fix: no reaction was offered for an enemy, so this failed outright
    useReactionOf nineOfRods
    assert nineOfRods.exhausted

    -- the cancelled card is shuffled back in and a replacement drawn -- it is the
    -- only card in the deck, so exactly one Swarm of Rats spawns, never two
    selectCount (InPlayEnemy AnyEnemy) `shouldReturn` 1

  it "still offers its reaction for a drawn non-weakness treachery" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    nineOfRods <- self `putAssetIntoPlay` Assets.nineOfRods3
    frozenInFear <- genEncounterCard Treacheries.frozenInFear
    run $ SetEncounterDeck (Deck [frozenInFear])
    run $ drawEncounterCard self.id GameSource

    useReactionOf nineOfRods
    assert nineOfRods.exhausted
