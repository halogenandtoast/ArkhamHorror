{- | Mechanics the terror decks need that the effect vocabulary cannot express:
taking terror back off a neighborhood, spawning a monster that is not one of the
human cultists, and the several cards that make an ally bear the cost.
-}
module AH3e.Content.UnderDarkWaves.TerrorBehaviors (behaviors) where

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
import AH3e.Types.Ids
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("discard-two-terror", discardTerror 2)
          , ("spawn-non-human", spawnNonHuman False)
          , ("spawn-non-human-here", spawnNonHuman True)
          , ("terror-ally-takes-the-harm", allyTakesTheHarm 1 1)
          , ("terror-discard-ally-or-doom", discardAllyOrDoom)
          , ("terror-ally-suffers-two", allyNearbySuffers False 2 0)
          , ("terror-ally-in-space-suffers-two", allyNearbySuffers True 2 0)
          ]
    }

{- | "Discard two terror from your neighborhood." Terror tokens only -- the cards
attached to the deck are discarded by being resolved, not by this.
-}
discardTerror :: Int -> EffectCtx -> GameM ()
discardTerror n ctx = do
  mnid <- investigatorNeighborhood ctx.investigator
  for_ mnid \nid -> do
    before <- (.terror) <$> getNeighborhood nid
    let taken = min n before
    when (taken > 0) do
      neighborhoodL nid . #terror -= taken
      logText ("Terror in " <> coerce nid <> " falls by " <> tshow taken)

{- | Draw from the bottom of the monster deck until one that is not Human turns
up, shuffling the humans passed over back in (the shape of rule 491.3b, inverted).
-}
drawNonHuman :: GameM (Maybe CardId)
drawNonHuman = go []
 where
  go passed = do
    deck <- use (#decks . #monster)
    case drawBottom deck of
      Nothing -> putBack passed >> pure Nothing
      Just (cid, rest) -> do
        #decks . #monster .= rest
        d <- monsterDef cid
        if "Human" `elem` d.traits
          then go (cid : passed)
          else putBack passed >> pure (Just cid)
  putBack passed = unless (null passed) do
    shuffled <- shuffle passed
    #decks . #monster %= (shuffled <>)

-- | "Spawn one non-human monster", either where its own card says or in your space.
spawnNonHuman :: Bool -> EffectCtx -> GameM ()
spawnNonHuman inYourSpace ctx =
  drawNonHuman >>= \case
    Nothing -> logText "No non-human monster remains in the deck"
    Just mid -> do
      d <- monsterDef mid
      spaces <-
        if inYourSpace
          then maybeToList <$> investigatorSpace ctx.investigator
          else ruleSpaces (Just mid) d.spawn
      case spaces of
        [] -> #decks . #monster %= (mid :)
        _ ->
          chooseGroup "Choose where the monster spawns"
            $ spaceChoices spaces \s -> [PlaceMonster mid s Ready]

{- | "You must assign this damage and horror to an ally, if possible." The harm
goes straight onto an ally of the reader's choosing, and only falls on them when
they hold none.
-}
allyTakesTheHarm :: Int -> Int -> EffectCtx -> GameM ()
allyTakesTheHarm dmg hor ctx = do
  allies <- matchingAssets ctx.investigator AllyCard
  case allies of
    [] -> push (SufferHarm ctx.investigator ctx.source NormalHarm dmg hor)
    _ ->
      chooseFor
        ctx.investigator
        "Choose an ally to bear it"
        [Choice (CardLabel c) [HarmAsset c dmg hor] | c <- allies]

-- | "Discard one ally; if you cannot, place one doom in your space."
discardAllyOrDoom :: EffectCtx -> GameM ()
discardAllyOrDoom ctx = do
  allies <- matchingAssets ctx.investigator AllyCard
  case allies of
    [] -> investigatorSpace ctx.investigator >>= traverse_ (push . PlaceDoom ctx.source)
    _ ->
      chooseFor
        ctx.investigator
        "Discard an ally"
        [Choice (CardLabel c) [DiscardAsset c] | c <- allies]

{- | "An ally suffers two damage", and the variant that reaches any ally in your
space rather than only your own.
-}
allyNearbySuffers :: Bool -> Int -> Int -> EffectCtx -> GameM ()
allyNearbySuffers inSpace dmg hor ctx = do
  owners <-
    if inSpace
      then investigatorSpace ctx.investigator >>= maybe (pure []) investigatorsAt
      else pure <$> getInvestigator ctx.investigator
  allies <- filterM (cardMatches AllyCard) (concatMap (.assets) owners)
  case allies of
    [] -> logText "No ally is there to suffer it"
    _ ->
      chooseFor
        ctx.investigator
        "Choose an ally"
        [Choice (CardLabel c) [HarmAsset c dmg hor] | c <- allies]
