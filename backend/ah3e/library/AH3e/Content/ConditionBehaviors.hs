{- | Condition mechanics. Blessed and cursed are handled by the engine (they
change the success threshold and are spent by a test); everything here hangs
off a card's own reckoning.
-}
module AH3e.Content.ConditionBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

darkPacts :: [CardCode]
darkPacts =
  [ "dark-pact-the-world-undone"
  , "dark-pact-pact-of-sacrifice"
  , "dark-pact-the-ultimate-price"
  , "dark-pact-an-alliance-of-evil"
  , "dark-pact-dark-destiny"
  , "dark-pact-forbidden-knowledge"
  , "dark-pact-tainted"
  , "dark-pact-wanted"
  ]

behaviors :: Behaviors
behaviors =
  mempty
    { assets = Map.fromList [(c, darkPactBehavior) | c <- darkPacts]
    , customEffects =
        Map.fromList
          [ ("dark-pact-reckoning", darkPactReckoning)
          , ("flip-condition", flipCondition)
          , ("discard-condition", discardCondition)
          , ("pact-of-sacrifice", pactOfSacrifice)
          , ("an-alliance-of-evil", allianceOfEvil)
          , ("an-alliance-of-evil-attacks", allianceOfEvilAttacks)
          , ("forbidden-knowledge", forbiddenKnowledge 3)
          , ("forbidden-knowledge:2", forbiddenKnowledge 2)
          , ("forbidden-knowledge:1", forbiddenKnowledge 1)
          , ("forbidden-knowledge:0", forbiddenKnowledge 0)
          ]
    }

darkPactBehavior :: AssetBehavior
darkPactBehavior = defaultAssetBehavior & #reckoning ?~ Custom "dark-pact-reckoning"

discardSelf :: Effect
discardSelf = Custom "discard-condition"

{- | What the exhausted side of a dark pact does the moment it is revealed.
Tainted and Wanted are conditions in their own right, so they simply stay in
play and take over the card's reckoning.
-}
revealedSide :: CardCode -> Maybe Effect
revealedSide = \case
  "dark-pact-the-world-undone" -> Just (Seq [PlaceDoomAt YourSpace (N 3), discardSelf])
  "dark-pact-the-ultimate-price" -> Just BecomeDevoured
  "dark-pact-dark-destiny" -> Just (Seq [DrawMythosTokens 6, discardSelf])
  "dark-pact-pact-of-sacrifice" -> Just (Custom "pact-of-sacrifice")
  "dark-pact-an-alliance-of-evil" -> Just (Custom "an-alliance-of-evil")
  "dark-pact-forbidden-knowledge" -> Just (Custom "forbidden-knowledge")
  _ -> Nothing

-- | The reckoning of a revealed side, for the two that keep the card in play.
revealedReckoning :: CardCode -> Maybe Effect
revealedReckoning = \case
  "dark-pact-tainted" -> Just (Custom "flip-condition")
  "dark-pact-wanted" ->
    Just
      ( Test
          Influence
          0
          (ByResult [((2, Nothing), discardSelf)])
          (Custom "flip-condition")
      )
  _ -> Nothing

sourceCard :: EffectCtx -> Maybe CardId
sourceCard ctx = case ctx.source of
  SourceCard cid -> Just cid
  _ -> Nothing

-- rule 474: a roll outside a test, so nothing can reroll or modify it
darkPactReckoning :: EffectCtx -> GameM ()
darkPactReckoning ctx = for_ (sourceCard ctx) \cid -> do
  a <- use (assetL cid)
  code <- cardCode cid
  if a.flipped
    then for_ (revealedReckoning code) (push . ResolveEffect ctx)
    else do
      i <- getInvestigator ctx.investigator
      codes <- traverse cardCode i.assets
      let dice = if "dark-blessing" `elem` codes then 2 else 1
      vs <- replicateM dice rollDie
      logText ("Dark pact: rolled " <> tshow vs)
      when (1 `elem` vs) do
        logText "Your debt has come due"
        push (ResolveEffect ctx (Custom "flip-condition"))

flipCondition :: EffectCtx -> GameM ()
flipCondition ctx = for_ (sourceCard ctx) \cid -> do
  assetL cid . #flipped %= not
  a <- use (assetL cid)
  code <- cardCode cid
  when a.flipped $ for_ (revealedSide code) (push . ResolveEffect ctx)

discardCondition :: EffectCtx -> GameM ()
discardCondition ctx = for_ (sourceCard ctx) (push . DiscardAsset)

-- "Choose another investigator on any space. That investigator is devoured."
pactOfSacrifice :: EffectCtx -> GameM ()
pactOfSacrifice ctx = do
  others <- filter ((/= ctx.investigator) . (.id)) <$> playingInvestigators
  case others of
    [] -> logText "No other investigator to sacrifice"
    _ ->
      chooseFor ctx.investigator "Choose an investigator to be devoured"
        $ [ Choice (InvestigatorLabel o.id) [DevourInvestigator o.id, DiscardAsset cid]
          | o <- others
          , cid <- toList (sourceCard ctx)
          ]

{- | "Spawn one monster in each space in your neighborhood. Each monster
recovers all of its health and deals damage and horror to the investigator it
has engaged." The heal only means anything for monsters already on the board,
so the second sentence covers every monster, not just the new ones.
-}
allianceOfEvil :: EffectCtx -> GameM ()
allianceOfEvil ctx = do
  mnid <- investigatorNeighborhood ctx.investigator
  board <- use #board
  let spaces = maybe [] (`neighborhoodSpaces` board) mnid
  pushAll
    ( [SpawnMonsterAt (Just sid) False | sid <- spaces]
        <> [ResolveEffect ctx (Custom "an-alliance-of-evil-attacks")]
    )

allianceOfEvilAttacks :: EffectCtx -> GameM ()
allianceOfEvilAttacks ctx = do
  #monsters . traverse . #damage .= 0
  logText "Every monster recovers all of its health"
  ms <- uses #monsters Map.elems
  let engaged m = case m.state of
        Engaged is -> is
        _ -> []
  pushAll
    ( [MonsterAttacks m.card i | m <- ms, i <- engaged m]
        <> [DiscardAsset cid | cid <- toList (sourceCard ctx)]
    )

{- | "Discard three clues total from among all investigators and the scenario
sheet (or all such clues if there are fewer than three). If exactly zero or one
clue is discarded this way, place one doom on the scenario sheet." The remaining
count is carried in the effect key, so the number discarded is 3 - remaining.
-}
forbiddenKnowledge :: Int -> EffectCtx -> GameM ()
forbiddenKnowledge remaining ctx = do
  invs <- playingInvestigators
  sheet <- use #sheetClues
  let sources =
        [Choice (InvestigatorLabel i.id) [DiscardClue (Just i.id)] | i <- invs, i.clues > 0]
          <> [Choice (TextLabel "Scenario sheet") [DiscardClue Nothing] | sheet > 0]
      next = Custom ("forbidden-knowledge:" <> tshow (remaining - 1))
  if remaining <= 0 || null sources
    then do
      let discarded = 3 - remaining
      when (discarded <= 1) do
        logText "Too little knowledge is given up; one doom goes on the scenario sheet"
        push (PlaceDoomOnSheet 1)
      for_ (sourceCard ctx) (push . DiscardAsset)
    else
      chooseFor
        ctx.investigator
        "Discard a clue"
        [Choice l (ms <> [ResolveEffect ctx next]) | Choice l ms <- sources]
