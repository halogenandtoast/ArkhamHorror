module Arkham.Location.Cards.TheDunwichLegacy.WhereDoomAwaits.FrozenSpringSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Location.CardDefs.TheDunwichLegacy.WhereDoomAwaits qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "Frozen Spring" do
  let
    revealFrozenSpring :: Investigator -> TestAppT LocationId
    revealFrozenSpring self = do
      here <- testLocation
      self `moveTo` here
      (frozenSpring, placement) <- placeLocationCard Locations.frozenSpring
      run placement
      updateProp @"remainingActions" 3 self
      run $ RevealLocation Nothing frozenSpring
      pure frozenSpring

  it
    "Forced - After you reveal Frozen Spring: Lose the remainder of your actions and immediately end your turn"
    . gameTest
    $ \self -> do
      _frozenSpring <- revealFrozenSpring self
      self.remainingActions `shouldReturn` 3
      resolveForcedAbility
      self.remainingActions `shouldReturn` 0
      (gameTurnPlayerInvestigatorId <$> getGame) `shouldReturn` Nothing

  {- #6018: the ability is resolving inside its own still-open reveal window when EndTurn
  arrives, and EndTurn used to drop every PerWindow use record unconditionally. The window's
  trailing @Do (CheckWindows ws)@ then re-derived the ability, which ended the turn again,
  which dropped the record again -- the issue's export traces eight identical cycles,
  dragging the game through Investigation, Enemy and Upkeep with no way for the player out.
  -}
  it "does not offer its Forced ability again once it has ended the turn" . gameTest $ \self -> do
    frozenSpring <- revealFrozenSpring self
    resolveForcedAbility
    assertNoAbilityOf frozenSpring

  {- The record EndTurn now keeps has to be self-cleaning: EndCheckWindow prunes it when the
  reveal window finally closes. @onlyOnce@ (a PerGame bucket) was never a valid patch for the
  loop -- The Spaces Between turns over every non-Sentinel-Hill location in this very
  scenario, and the Diverging Paths' Forced must fire again when they are re-revealed. -}
  it "fires again when the location is unrevealed and revealed again" . gameTest $ \self -> do
    frozenSpring <- revealFrozenSpring self
    resolveForcedAbility
    run $ UnrevealLocation frozenSpring
    run $ Do BeginRound
    self.remainingActions `shouldSatisfyM` (> 0)
    run $ RevealLocation Nothing frozenSpring
    resolveForcedAbility
    self.remainingActions `shouldReturn` 0
