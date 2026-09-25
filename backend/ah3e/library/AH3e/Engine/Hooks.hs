module AH3e.Engine.Hooks where

import AH3e.Content.Behaviors
import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

assetBehavior :: CardId -> GameM AssetBehavior
assetBehavior cid = do
  code <- cardCode cid
  pure $ Map.findWithDefault defaultAssetBehavior code behaviors.assets

codexBehavior :: ArchiveNumber -> CodexBehavior
codexBehavior n = Map.findWithDefault defaultCodexBehavior n behaviors.codex

investigatorBehavior :: InvestigatorId -> InvestigatorBehavior
investigatorBehavior iid = Map.findWithDefault defaultInvestigatorBehavior iid behaviors.investigators

customEffect :: Text -> Maybe (EffectCtx -> GameM ())
customEffect key = Map.lookup key behaviors.customEffects

customActivation :: Text -> Maybe (CardId -> GameM ())
customActivation key = Map.lookup key behaviors.customActivations

codexEntry :: ArchiveNumber -> GameM (Maybe CodexEntry)
codexEntry n = uses #codex (listToMaybe . filter ((== n) . (.number)))

componentActionsFor :: InvestigatorId -> GameM [(ComponentRef, Int, ComponentActionDef)]
componentActionsFor iid = do
  i <- getInvestigator iid
  let sheet = [(SheetRef iid, n, a) | (n, a) <- zip [0 ..] (investigatorBehavior iid).componentActions]
  cards <- fmap concat $ for i.assets \cid -> do
    b <- assetBehavior cid
    pure [(CardRef cid, n, a) | (n, a) <- zip [0 ..] b.componentActions, cid `notElem` i.lockedAssets]
  codex <- use #codex
  let codexActions =
        [ (CodexRef e.number, n, a)
        | e <- codex
        , (n, a) <- zip [0 ..] (codexBehavior e.number).componentActions
        ]
  pure (sheet <> cards <> codexActions)

lookupComponentAction :: InvestigatorId -> ComponentRef -> Int -> GameM (Maybe ComponentActionDef)
lookupComponentAction iid ref n = do
  as <- componentActionsFor iid
  pure $ listToMaybe [a | (r, k, a) <- as, r == ref, k == n]

hasAssetWith :: InvestigatorId -> (AssetBehavior -> Bool) -> GameM Bool
hasAssetWith iid p = do
  i <- getInvestigator iid
  anyM (fmap p . assetBehavior) [c | c <- i.assets, c `notElem` i.lockedAssets]

-- rule 492.2, widened by cards that trade within the neighborhood
tradePartners :: InvestigatorId -> GameM [Investigator]
tradePartners iid = do
  wide <- hasAssetWith iid (.tradeInNeighborhood)
  msid <- investigatorSpace iid
  mnid <- investigatorNeighborhood iid
  board <- use #board
  others <- filter ((/= iid) . (.id)) <$> playingInvestigators
  pure
    [ o
    | o <- others
    , o.space == msid || (wide && isJust mnid && (o.space >>= (`spaceNeighborhood` board)) == mnid)
    ]

-- rules 402.4, 402.5, 409, 429.5, 436, 492
legalActions :: InvestigatorId -> GameM [ActionKind]
legalActions iid = do
  i <- getInvestigator iid
  restricted <- isRestrictedByEngagement iid
  sid <- maybe (error "investigator not on board") pure i.space
  here <- monstersAt sid
  space <- getSpace sid
  engaged <- engagedMonsters iid
  partners <- tradePartners iid
  let unfocused = [s | s <- allSkills, Map.findWithDefault 0 s i.focus == 0]
      basic =
        [MoveAction | not restricted]
          <> [GatherResourcesAction | not restricted]
          <> [FocusAction | not (null unfocused)]
          -- warding takes doom off your own space, so it needs at least one there
          <> [WardAction | not restricted, space.doom > 0]
          <> [AttackAction | not (null here)]
          <> [EvadeAction | not (null engaged)]
          -- 470.2: research moves your own clues to the scenario sheet, so it needs at least one
          <> [ResearchAction | not restricted, i.clues > 0]
          <> [TradeAction | not restricted, not (null partners)]
  components <- componentActionsFor iid
  comps <- fmap catMaybes $ for components \(ref, n, a) -> do
    ok <- a.canPerform iid
    pure
      $ if ok && (not restricted || a.allowedWhileEngaged)
        then Just (ComponentAction ref n)
        else Nothing
  pure $ filter (`notElem` i.performed) (basic <> comps)

reactionsFor :: Trigger -> GameM [Reaction]
reactionsFor trigger = do
  let iid = triggerInvestigator trigger
  playing <- investigatorIsPlaying iid
  if not playing
    then pure []
    else do
      i <- getInvestigator iid
      sheet <- (investigatorBehavior iid).reactions iid trigger
      cards <- fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
        b <- assetBehavior cid
        b.reactions cid trigger
      pure (sheet <> cards)

blockedSpaces :: GameM [SpaceId]
blockedSpaces = do
  codex <- use #codex
  concat <$> for codex \e -> (codexBehavior e.number).blockedSpaces e

reachable :: [SpaceId] -> GameM [SpaceId]
reachable sids = do
  blocked <- blockedSpaces
  pure (filter (`notElem` blocked) sids)

codexSpaceEncounter :: SpaceId -> GameM (Maybe Effect)
codexSpaceEncounter sid = do
  codex <- use #codex
  pure $ listToMaybe (mapMaybe (\e -> (codexBehavior e.number).spaceEncounter e sid) codex)

-- | Actions an investigator may take on their turn: the usual two (402.2), plus
-- bonuses earned this phase, plus additional actions from what they hold (402.3).
-- A card traded over after being used this round is locked and adds nothing.
actionAllowance :: InvestigatorId -> GameM Int
actionAllowance iid = do
  i <- getInvestigator iid
  extra <- sum <$> for [c | c <- i.assets, c `notElem` i.lockedAssets] (fmap (.extraActions) . assetBehavior)
  pure (2 + i.bonusActions + extra)
