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
          , ("la-bella-luna-dice", laBellaLunaDice)
          , ("magick-shoppe-spells", magickShoppeSpells)
          , ("soto-spell-market:any", spellMarket 3 Nothing FullPrice)
          , ("soto-spell-market:one-half", spellMarket 3 (Just 1) HalfPrice)
          , ("soto-spell-market-close", spellMarketClose)
          ]
    }

{- | "Reveal the top three spells from the deck. You may buy ... them. Place the rest
on the bottom of the deck. If you buy anything, gain one clue from your neighborhood."
The clue is what buying from the display calls its @ifBought@ effect, which 'BuyFromDeck'
has no room for, so the spells in hand are counted on the way in and again once the
shelves close.
-}
spellMarket :: Int -> Maybe Int -> Pricing -> EffectCtx -> GameM ()
spellMarket n limit pricing ctx = do
  held <- length <$> matchingAssets ctx.investigator SpellCard
  #sheetTokens . at marketKey ?= held
  pushAll
    [ ResolveEffect ctx (BuyFromDeck SpellDeckKind n limit pricing)
    , ResolveEffect ctx (Custom "soto-spell-market-close")
    ]

spellMarketClose :: EffectCtx -> GameM ()
spellMarketClose ctx = do
  before <- uses #sheetTokens (Map.findWithDefault 0 marketKey)
  #sheetTokens . at marketKey .= Nothing
  held <- length <$> matchingAssets ctx.investigator SpellCard
  when (held > before) $ push (ResolveEffect ctx (GainE ClueFromNeighborhood))

marketKey :: Text
marketKey = "soto-spells-held"

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

{- | "Roll four dice; gain $1 for each odd number you roll." A roll outside a test
(rule 474), so it is resolved where it is made and nothing can change it.
-}
laBellaLunaDice :: EffectCtx -> GameM ()
laBellaLunaDice ctx = do
  rolls <- replicateM 4 rollDie
  let won = length (filter odd rolls)
  logText ("Rolled " <> tshow rolls <> " and gains $" <> tshow won)
  addMoney ctx.investigator won

{- | "Reveal the top three spells in the deck. You may buy one of them or become
FATIGUED to gain one of them. Return the rest to the bottom of the deck." A spell
with no printed value cannot be bought, and nor can one beyond their means.
-}
magickShoppeSpells :: EffectCtx -> GameM ()
magickShoppeSpells ctx = do
  let iid = ctx.investigator
  deck <- use (#decks . #spell)
  let (revealed, rest) = splitAt 3 deck
  #decks . #spell .= rest
  names <- for revealed \cid -> (.name) <$> getCardDef cid
  unless (null revealed) $ logText ("Miriam lays out " <> T.intercalate ", " names)
  purse <- availableMoney iid
  tired <- canPayCost iid (CostCondition "FATIGUED")
  priced <- for (zip revealed names) \(cid, name) -> (cid,name,) <$> cardValue cid
  let keeping cid = ReturnToBottom SpellDeckKind (filter (/= cid) revealed)
  chooseFor iid "Buy a spell, or wear yourself out for one"
    $ [ Choice (CardsLabel ("Buy " <> name <> " for $" <> tshow v) [cid]) [BuyCard iid cid v, keeping cid]
      | (cid, name, Just v) <- priced
      , v <= purse
      ]
    <> [ Choice
           (CardsLabel ("Become FATIGUED for " <> name) [cid])
           [PayCost ctx (CostCondition "FATIGUED"), GainAsset iid cid, keeping cid]
       | tired
       , (cid, name, _) <- priced
       ]
    <> [Choice (DoneLabel "Take none") [ReturnToBottom SpellDeckKind revealed]]
