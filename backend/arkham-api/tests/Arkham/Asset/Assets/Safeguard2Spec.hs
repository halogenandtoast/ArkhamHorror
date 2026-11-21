module Arkham.Asset.Assets.Safeguard2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Window (defaultWindows, mkAfter)
import Arkham.Window qualified as Window
import TestImport.New

spec :: Spec
spec = describe "Safeguard (2)" do
  context "for the remainder of that investigator's turn" do
    it "you may move to a connecting location they move to" . gameTest $ \self -> do
      roland <- addInvestigator rolandBanks
      (location1, location2) <- testConnectedLocations id id
      updateProp @"clues" 0 location1
      updateProp @"clues" 0 location2
      self `moveTo` location1
      roland `moveTo` location1
      run $ RevealLocation Nothing (toId location2)

      safeguard2 <- roland `putAssetIntoPlay` Assets.safeguard2
      [useSafeguard] <- roland `getActionsFrom` safeguard2
      roland `useAbility` useSafeguard

      pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
      [usePendant] <- self `getActionsFrom` pendantOfTheQueen
      -- location2 is the only revealed location Pendant can send you to, and
      -- moving is the only thing it can do there, so both choices auto-resolve
      self `useAbility` usePendant

      self.location `shouldReturn` Just (toId location2)
      chooseOptionMatching "move too" \case
        Label lbl _ -> lbl == "$label.moveToo"
        _ -> False
      roland.location `shouldReturn` Just (toId location2)

    it "you may not move to a location that does not connect to yours" . gameTest $ \self -> do
      roland <- addInvestigator rolandBanks
      (location1, _location2) <- testConnectedLocations id id
      location3 <- testLocation
      updateProp @"clues" 0 location1
      updateProp @"clues" 0 location3
      self `moveTo` location1
      roland `moveTo` location1
      run $ RevealLocation Nothing (toId location3)

      safeguard2 <- roland `putAssetIntoPlay` Assets.safeguard2
      [useSafeguard] <- roland `getActionsFrom` safeguard2
      roland `useAbility` useSafeguard

      pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
      [usePendant] <- self `getActionsFrom` pendantOfTheQueen
      -- location3 is the only revealed location Pendant can send you to, and
      -- moving is the only thing it can do there, so both choices auto-resolve
      self `useAbility` usePendant

      self.location `shouldReturn` Just (toId location3)
      roland.location `shouldReturn` Just (toId location1)
      questions <- toList . gameQuestion <$> getGame
      liftIO $ show questions `shouldNotContain` "moveToo"

  -- The reaction's default PlayerLimit PerWindow 1 was recorded at window depth 0 and so
  -- never pruned, and its DuringTurn matcher sees windows that recur verbatim every turn
  -- (When FastPlayerWindow, When TurnBegins <iid>), so one arming killed it for the rest
  -- of the scenario. #5999
  it "can be armed again on a later turn" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    location1 <- testLocation
    self `moveTo` location1
    roland `moveTo` location1

    safeguard2 <- roland `putAssetIntoPlay` Assets.safeguard2
    [useSafeguard] <- roland `getActionsFrom` safeguard2
    roland `useAbility` useSafeguard

    -- the turn ends and upkeep readies it, so only the stale use record could be in the way
    run $ EndTurn (toId self)
    run $ Ready (toTarget safeguard2)

    (roland `getActionsFrom` safeguard2) `shouldSatisfyM` notNull

  {- The #6018 fix narrowed that purge rather than removing it: a PerWindow record now
  survives EndTurn while the queue still owes a `Do (CheckWindows ws)` for the record's OWN
  window. Ending a turn for real always leaves such a check pending -- `resolveWithWindow`
  queues the `#after TurnEnds` check behind EndTurn -- so a guard that kept records whenever
  ANY check was pending would bring #5999 straight back. Unlike the test above, the use here
  records the windows the limit actually counts against; `useAbility` passes none at all. -}
  it "can be armed again even with an unrelated window check still pending" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    location1 <- testLocation
    self `moveTo` location1
    roland `moveTo` location1

    safeguard2 <- roland `putAssetIntoPlay` Assets.safeguard2
    [useSafeguard] <- roland `getActionsFrom` safeguard2
    run $ UseAbility (toId roland) useSafeguard (defaultWindows $ toId roland)
    run $ Ready (toTarget safeguard2)

    -- readied again, so the use record is the only thing that can keep it off the list
    (roland `getActionsFrom` safeguard2) `shouldReturn` []

    pushAndRunAll [EndTurn (toId self), CheckWindows [mkAfter $ Window.TurnEnds (toId self)]]
    run $ Ready (toTarget safeguard2)

    (roland `getActionsFrom` safeguard2) `shouldSatisfyM` notNull
