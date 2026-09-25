module Arkham.UltimatumsAndBoons.BoonOfTheChildSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Helpers.UltimatumsAndBoons
import TestImport.New

boonSource :: Source
boonSource = UltimatumOrBoonSource (Boon BoonOfTheChild)

-- N.B. availability is asked through getActions (what the player window uses),
-- not through the ability matcher: getGameAbilities collects entity abilities
-- only, so `select (AbilityIs (UltimatumOrBoonSource ...) n)` never matches a
-- boon even while the boon's ability is being offered.
spec :: Spec
spec = describe "Boon of the Child" $ do
  it "once per round you may play the topmost event of your discard, bottom-decking it" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfTheChild]
    eventA <- genPlayerCardWith Events.emergencyCache (setPlayerCardOwner (toId self))
    eventB <- genPlayerCardWith Events.emergencyCache (setPlayerCardOwner (toId self))
    withProp @"discard" [eventA, eventB] self
    withDeck self [Assets.flashlight]

    duringRound do
      duringTurn self do
        getActionsFrom self boonSource `shouldSatisfyM` notNull
        inWindow self $ useFastActionOf boonSource 1
        self.resources `shouldReturn` 3
        -- Emergency Cache is not fast, so the play still costs an action
        self.remainingActions `shouldReturn` 2
        -- it goes to the bottom of the deck instead of the discard pile
        asDefs self.discard `shouldReturn` [Events.emergencyCache]
        asDefs self.deck `shouldReturn` [Assets.flashlight, Events.emergencyCache]
        -- and the permission is spent for the rest of the round. "An investigator
        -- may play" is group-wide, hence a GroupLimit rather than a player limit
        getActionsFrom self boonSource `shouldSatisfyM` null

    duringRound do
      duringTurn self do
        -- the permission returns next round
        getActionsFrom self boonSource `shouldSatisfyM` notNull
