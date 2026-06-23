module Arkham.Investigator.Cards.CarsonSinclairSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = describe "Carson Sinclair" do
  context "granted action" do
    -- #4894: the granted "as if it were your turn" action may only be spent on a
    -- real action. It is not actually the recipient's turn, so Fast cards (e.g.
    -- Shortcut, whose window is "during your turn") must not be playable with it --
    -- otherwise the Fast play consumes nothing yet wastes the granted action.
    it "does not let the recipient play a Fast card" . gameTestWith Investigators.carsonSinclair $ \self -> do
      roland <- addInvestigator rolandBanks
      (location1, _location2) <- testConnectedLocations id id
      self `moveTo` location1
      roland `moveTo` location1
      shortcut <- genMyCard roland Events.shortcut
      addToHand roland shortcut

      duringTurn self do
        -- Isolate the granted action: Roland has no actions of his own (it is not
        -- his turn), so the only action available is the one Carson grants.
        roland `loseActions` 3

        [grant] <- self `getActionsFrom` self
        self `useAbility` grant
        chooseTarget roland

        -- The granted window must not offer the Fast Shortcut play...
        assertNotTarget (toCardId shortcut)

        -- ...but the granted action is still usable on a real action (taking a
        -- resource consumes it), and Shortcut stays in hand (never played).
        chooseOptionMatching "take resource as granted action" \case
          ResourceLabel iid _ -> iid == toId roland
          _ -> False

        roland.remainingActions `shouldReturn` 0
        roland.hand `shouldReturn` [toCard shortcut]
