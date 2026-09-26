module AH3e.Engine.Hooks where

import AH3e.Content.Behaviors
import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.List (nub)
import Data.Map.Strict qualified as Map

assetBehavior :: CardId -> GameM AssetBehavior
assetBehavior cid = do
  code <- cardCode cid
  pure $ Map.findWithDefault defaultAssetBehavior code behaviors.assets

monsterBehavior :: CardId -> GameM MonsterBehavior
monsterBehavior mid = do
  code <- cardCode mid
  pure $ Map.findWithDefault defaultMonsterBehavior code behaviors.monsters

codexBehavior :: ArchiveNumber -> CodexBehavior
codexBehavior n = Map.findWithDefault defaultCodexBehavior n behaviors.codex

investigatorBehavior :: InvestigatorId -> InvestigatorBehavior
investigatorBehavior iid = Map.findWithDefault defaultInvestigatorBehavior iid behaviors.investigators

customAfterTest :: Text -> Maybe (Source -> Int -> GameM ())
customAfterTest key = Map.lookup key behaviors.customAfterTests

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

{- | Harm on its way onto a card, less whatever the card itself prevents. The
prevented harm is gone rather than moved, and the card counts as used.
-}
preventOwnHarm :: InvestigatorId -> CardId -> (Int, Int) -> GameM (Int, Int)
preventOwnHarm iid cid (dmg, hor) = do
  b <- assetBehavior cid
  used <- usedThisRound cid iid
  case b.preventsOwnHarm of
    Just (stat, threshold, amount) | not used -> do
      let taken = case stat of DamageStat -> dmg; HorrorStat -> hor
      if taken < threshold
        then pure (dmg, hor)
        else do
          name <- (.name) <$> getCardDef cid
          let what = case stat of DamageStat -> " damage"; HorrorStat -> " horror"
          logText (name <> " prevents " <> tshow amount <> what)
          investigatorL iid . #usedAssets %= (<> [cid])
          pure case stat of
            DamageStat -> (dmg - amount, hor)
            HorrorStat -> (dmg, hor - amount)
    _ -> pure (dmg, hor)

{- | What anyone's cards do about a monster that just took damage. Every
investigator in play is asked, since the card need not belong to whoever dealt it.
-}
afterMonsterDamagedFor :: CardId -> Source -> GameM [Message]
afterMonsterDamagedFor mid src = do
  invs <- playingInvestigators
  fmap concat $ for invs \i ->
    fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
      b <- assetBehavior cid
      b.afterMonsterDamaged cid i.id mid src

{- | What the sufferer's cards do about a harm plan that has landed. A card the
harm destroyed is still asked, since it did suffer what destroyed it.
-}
afterHarmFor :: HarmPlan -> GameM [Message]
afterHarmFor plan = do
  i <- getInvestigator plan.investigator
  let assigned = [cid | Just (cid, _) <- [plan.damageTo, plan.horrorTo]]
      cards = nub ([c | c <- i.assets, c `notElem` i.lockedAssets] <> assigned)
  fmap concat $ for cards \cid -> do
    b <- assetBehavior cid
    b.afterHarm cid plan.investigator plan

-- | What this investigator's cards do about clues they just gained.
afterGainClueFor :: InvestigatorId -> GameM [Message]
afterGainClueFor iid = do
  i <- getInvestigator iid
  fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    b.afterGainClue cid iid

{- | Whether this monster passes this investigator by: a non-epic monster, an
investigator wearing something that hides them, and no provocation from them yet.
-}
monsterIgnores :: CardId -> InvestigatorId -> GameM Bool
monsterIgnores mid iid = do
  d <- monsterDef mid
  hidden <- hasAssetWith iid (.ignoredByMonsters)
  angered <- uses #provoked (elem iid . Map.findWithDefault [] mid)
  pure (hidden && not d.epic && not angered)

-- | Cards that could halve a purchase for this investigator, with their names.
halfPriceCards :: InvestigatorId -> GameM [(CardId, Text)]
halfPriceCards iid = do
  i <- getInvestigator iid
  fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets, c `notElem` i.usedAssets] \cid -> do
    b <- assetBehavior cid
    name <- (.name) <$> getCardDef cid
    pure $ if b.halfPricePerRound then Just (cid, name) else Nothing

-- | Successes the tested investigator's cards add beyond one per passing die.
extraSuccessesFor :: TestState -> GameM Int
extraSuccessesFor ts = do
  i <- getInvestigator ts.investigator
  sum <$> for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    b.extraSuccesses cid ts.investigator ts

{- | What is offered while a test resolves: the tested investigator's own cards,
and the monster they are testing against, which may have text of its own.
-}
testOptionsFor :: TestState -> GameM [Reaction]
testOptionsFor ts = do
  i <- getInvestigator ts.investigator
  fromCards <- fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    b.testOptions cid ts.investigator ts
  fromMonster <- case ts.kind of
    ActionTest _ (Just mid) -> do
      present <- uses #monsters (Map.member mid)
      if present
        then do
          b <- monsterBehavior mid
          b.testOptions mid ts.investigator ts
        else pure []
    _ -> pure []
  pure (fromCards <> fromMonster)

{- | A monster's health as it stands: what its card prints, plus its elite health
per investigator, less whatever a card in play takes off it. Never below one, so a
monster is defeated by damage rather than by arithmetic.
-}
effectiveMonsterHealth :: CardId -> GameM (Maybe Int)
effectiveMonsterHealth mid = do
  base <- monsterHealth mid
  b <- monsterBehavior mid
  delta <- b.healthDelta mid
  pure (max 1 . (+ delta) <$> base)

-- | What the codex says about a monster arriving, or being defeated.
codexAboutMonster
  :: (CodexBehavior -> CodexEntry -> CardId -> GameM [Message]) -> CardId -> GameM [Message]
codexAboutMonster which mid = do
  codex <- use #codex
  fmap concat $ for codex \e -> which (codexBehavior e.number) e mid

-- | Cards anyone in play holds that may prevent the damage about to be suffered.
damagePreventionsFor :: HarmPlan -> GameM [(InvestigatorId, Reaction)]
damagePreventionsFor plan = do
  invs <- playingInvestigators
  fmap concat $ for invs \i ->
    fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets, c `notElem` i.usedAssets] \cid -> do
      b <- assetBehavior cid
      map (i.id,) <$> b.damagePrevention cid i.id plan

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

{- | Actions an investigator may take on their turn: the usual two (402.2), plus
bonuses earned this phase, plus additional actions from what they hold (402.3).
A card traded over after being used this round is locked and adds nothing.
-}
actionAllowance :: InvestigatorId -> GameM Int
actionAllowance iid = do
  i <- getInvestigator iid
  extra <-
    sum <$> for [c | c <- i.assets, c `notElem` i.lockedAssets] (fmap (.extraActions) . assetBehavior)
  pure (2 + i.bonusActions + extra)
