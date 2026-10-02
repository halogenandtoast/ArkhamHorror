-- | What Bound to Serve asks of the engine directly.
module AH3e.Content.SecretsOfTheOrder.BoundToServeBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("bound-to-serve-reckoning", reckoning)
          , ("bound-to-serve-spell-market", openMarket)
          , ("bound-to-serve-spell-market-close", closeMarket)
          ]
    }

{- | "For each spirit monster, place one doom in its space. If there are no spirit
monsters on the board, spawn one spirit monster." The spawn is found the way a trait
is (491.3b) and put back on the bottom, so the ordinary spawn draws it and everything
that answers a monster arriving still runs.
-}
reckoning :: EffectCtx -> GameM ()
reckoning _ = do
  spirits <- uses #monsters Map.elems >>= filterM (fmap isSpirit . monsterDef . (.card))
  if null spirits
    then
      revealMonstersFromBottom "Spirit" 1 >>= \case
        (mid : _) -> do
          #decks . #monster %= (<> [mid])
          push (SpawnMonsterAt Nothing False)
        [] -> logText "No spirit monster is left to answer the pact"
    else pushAll [PlaceDoom SourceScenario m.space | m <- spirits]
 where
  isSpirit d = "Spirit" `elem` d.traits

{- | "Reveal the top three spells from the deck. You may buy any number of them. Place
the rest on the bottom of the deck. If you buy anything, gain one clue from your
neighborhood." The clue is what buying from the display calls its @ifBought@ effect,
which 'BuyFromDeck' has no room for, so the spells in hand are counted on the way in
and again once the shelves close.
-}
openMarket :: EffectCtx -> GameM ()
openMarket ctx = do
  held <- length <$> matchingAssets ctx.investigator SpellCard
  #sheetTokens . at marketKey ?= held
  pushAll
    [ ResolveEffect ctx (BuyFromDeck SpellDeckKind 3 Nothing FullPrice)
    , ResolveEffect ctx (Custom "bound-to-serve-spell-market-close")
    ]

closeMarket :: EffectCtx -> GameM ()
closeMarket ctx = do
  before <- uses #sheetTokens (Map.findWithDefault 0 marketKey)
  #sheetTokens . at marketKey .= Nothing
  held <- length <$> matchingAssets ctx.investigator SpellCard
  when (held > before) $ push (ResolveEffect ctx (GainE ClueFromNeighborhood))

marketKey :: Text
marketKey = "bound-to-serve-spells-held"
