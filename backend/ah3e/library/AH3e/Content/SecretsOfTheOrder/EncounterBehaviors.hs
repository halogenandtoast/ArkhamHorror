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
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.State
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("gateway-onward", gatewayOnward True)
          , ("gateway-onward:must", gatewayOnward False)
          , ("wild-gateway-toll", wildGatewayToll)
          , ("spawn-inhuman-monster", spawnInhumanMonster)
          , ("witch-house-spells", witchHouseSpells)
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

{- | "Spawn one non-human monster." The monster is found the way a trait is
(491.3b), then put back on the bottom so the ordinary spawn draws it and
everything that answers a monster arriving still runs.
-}
spawnInhumanMonster :: EffectCtx -> GameM ()
spawnInhumanMonster _ = do
  found <- revealMonstersMatching (\d -> "Human" `notElem` d.traits) 1
  case found of
    (mid : _) -> do
      #decks . #monster %= (<> [mid])
      push (SpawnMonsterAt Nothing False)
    [] -> logText "Nothing inhuman is left in the monster deck"

{- | "Reveal the top three spells in the deck. You may gain a DARK PACT to gain one
of them. Place any spells you do not gain on the bottom of the deck." The price is a
condition rather than money, which the display's own buying cannot charge.
-}
witchHouseSpells :: EffectCtx -> GameM ()
witchHouseSpells ctx = do
  deck <- use (#decks . #spell)
  let (revealed, rest) = splitAt 3 deck
  #decks . #spell .= rest
  names <- for revealed \cid -> (.name) <$> getCardDef cid
  unless (null revealed) $ logText ("The voice offers " <> T.intercalate ", " names)
  pact <- canPayCost ctx.investigator (CostCondition "DARK PACT")
  chooseFor ctx.investigator "Gain a DARK PACT to take one of them?"
    $ [ Choice
          (CardLabel cid)
          [ PayCost ctx (CostCondition "DARK PACT")
          , GainAsset ctx.investigator cid
          , ReturnToBottom SpellDeckKind (filter (/= cid) revealed)
          ]
      | pact
      , cid <- revealed
      ]
    <> [Choice (DoneLabel "Take none") [ReturnToBottom SpellDeckKind revealed]]
