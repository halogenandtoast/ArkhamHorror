-- | What the Secrets of the Order encounter decks ask of the engine directly.
module AH3e.Content.SecretsOfTheOrder.EncounterBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("gateway-onward", gatewayOnward True)
          , ("gateway-onward:must", gatewayOnward False)
          , ("wild-gateway-toll", wildGatewayToll)
          ]
    }

-- | "You may move one space or to another wild gateway."
gatewayOnward :: Bool -> EffectCtx -> GameM ()
gatewayOnward mayStay ctx = do
  msid <- investigatorSpace ctx.investigator
  board <- use #board
  offerOnward ctx "Step through?" mayStay (maybe [] (`sameThresholdSpaces` board) msid) Nothing

{- | "You may spend one remnant or choose another investigator to become FATIGUED.
If you do, move one space or to another wild gateway." Their own fatigue will not
do, so the only offers are the remnant and somebody else; a lone investigator with
no remnant has nothing to pay and stays where they are.
-}
wildGatewayToll :: EffectCtx -> GameM ()
wildGatewayToll ctx = do
  let iid = ctx.investigator
  rich <- canPayCost iid (SpendRemnants 1)
  others <- filter ((/= iid) . (.id)) <$> playingInvestigators
  let go = ResolveEffect ctx (Custom "gateway-onward:must")
  chooseFor iid "The gateway demands an offering"
    $ [label "Spend one remnant" [PayCost ctx (SpendRemnants 1), go] | rich]
    <> [Choice (InvestigatorLabel o.id) [GainConditionMsg o.id "FATIGUED", go] | o <- others]
    <> [Choice (DoneLabel "Offer nothing") []]
