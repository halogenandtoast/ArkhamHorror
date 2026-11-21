module Arkham.Location.Cards.TheForgottenAge.Rainforest.OvergrownRuinsSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Location.CardDefs.TheForgottenAge.Rainforest qualified as Locations
import Arkham.Treachery.CardDefs.TheForgottenAge.Poison qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Overgrown Ruins" do
  let
    enterOvergrownRuins :: Investigator -> Bool -> TestAppT LocationId
    enterOvergrownRuins self poisoned = do
      here <- testLocation
      self `moveTo` here
      (overgrownRuins, placement) <- placeLocationCard Locations.overgrownRuins
      run placement
      run $ RevealLocation Nothing overgrownRuins
      when poisoned do
        poison <- genCard Treacheries.poisoned
        run $ CreateWeaknessInThreatArea poison (toId self)
      updateProp @"remainingActions" 3 self
      self `moveTo` overgrownRuins
      pure overgrownRuins

  it "Forced - After you enter this location, if you are poisoned: Lose all remaining actions and end your turn"
    . gameTest
    $ \self -> do
      _overgrownRuins <- enterOvergrownRuins self True
      self.remainingActions `shouldReturn` 3
      resolveForcedAbility
      self.remainingActions `shouldReturn` 0
      (gameTurnPlayerInvestigatorId <$> getGame) `shouldReturn` Nothing

  it "does nothing if you are not poisoned" . gameTest $ \self -> do
    overgrownRuins <- enterOvergrownRuins self False
    assertNoAbilityOf overgrownRuins
    self.remainingActions `shouldReturn` 3

  {- #6027, the same loop as Frozen Spring (#6018) on the other window in 'forcedLimit''s
  PerWindow bucket: the ability resolves inside its own still-open @Enters #after@ window, so
  the blanket PerWindow purge at EndTurn destroyed the use record that stops that window's
  trailing @Do (CheckWindows ws)@ re-deriving it. The issue's reporter clicked the forced
  button six times before giving up. -}
  it "does not offer its Forced ability again once it has ended the turn" . gameTest $ \self -> do
    overgrownRuins <- enterOvergrownRuins self True
    resolveForcedAbility
    assertNoAbilityOf overgrownRuins
