module AH3e.Engine.Hooks where

import AH3e.Content.Behaviors
import AH3e.Engine.Behavior
import AH3e.Engine.Helpers (hiddenForTheRound)
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

{- | The abilities a card offers during its owner's turn for free, with the
reference that names them.
-}
freeActionsFor :: InvestigatorId -> GameM [(ComponentRef, Int, ComponentActionDef)]
freeActionsFor iid = do
  i <- getInvestigator iid
  fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    fmap catMaybes $ for (zip [0 ..] b.freeActions) \(n, a) -> do
      ok <- a.canPerform iid
      pure (if ok then Just (CardRef cid, n, a) else Nothing)

{- | What anyone in reach offers to stop something being put down in their own
neighborhood; the first card to answer is asked.
-}
placementStops :: Placement -> SpaceId -> GameM [(InvestigatorId, Reaction)]
placementStops what sid = do
  board <- use #board
  let there = spaceNeighborhood sid board
  invs <- playingInvestigators
  fmap concat $ for invs \i -> do
    mine <- investigatorNeighborhood i.id
    if isNothing there || mine /= there
      then pure []
      else fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets, c `notElem` i.usedAssets] \cid -> do
        b <- assetBehavior cid
        map (i.id,) <$> b.stopsPlacement cid i.id what sid

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
      entries <- use #codex
      codex <- fmap concat $ for entries \e -> (codexBehavior e.number).reactions e iid trigger
      pure (sheet <> cards <> codex)

-- | What the codex does when an anomaly opens in a neighborhood, in codex order.
anomalyOpened :: NeighborhoodId -> GameM [Message]
anomalyOpened nid = do
  entries <- use #codex
  fmap concat $ for entries \e -> (codexBehavior e.number).afterAnomaly e nid

{- | The effect a codex card puts in place of the encounter about to be read, if
any; the first card to answer wins.
-}
encounterOverrideFor :: InvestigatorId -> Encounter -> GameM (Maybe Effect)
encounterOverrideFor iid enc = do
  entries <- use #codex
  overrides <- for entries \e -> (codexBehavior e.number).encounterOverride e iid enc
  pure (listToMaybe (catMaybes overrides))

{- | Cards that can simply prevent one harm this round, with their names, for the
offer the prevention step makes.
-}
harmPreventers :: InvestigatorId -> GameM [(CardId, Text)]
harmPreventers iid = do
  i <- getInvestigator iid
  fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets, c `notElem` i.usedAssets] \cid -> do
    b <- assetBehavior cid
    name <- (.name) <$> getCardDef cid
    pure $ if b.preventsOneHarmPerRound then Just (cid, name) else Nothing

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
  i <- getInvestigator iid
  held <-
    anyM
      (\cid -> assetBehavior cid >>= \b -> b.ignoredByMonsters cid iid)
      [c | c <- i.assets, c `notElem` i.lockedAssets]
  -- a card may buy the same thing for the rest of the round (On the Lam)
  laidLow <- usedAbility iid hiddenForTheRound
  let hidden = held || laidLow
  angered <- uses #provoked (elem iid . Map.findWithDefault [] mid)
  pure (hidden && not d.epic && not angered)

-- | Hands' worth of assets this investigator may use beyond the usual two.
handsAllowance :: InvestigatorId -> GameM Int
handsAllowance iid = do
  i <- getInvestigator iid
  sum <$> for [c | c <- i.assets, c `notElem` i.lockedAssets] (fmap (.handsDelta) . assetBehavior)

{- | What every investigator's cards do about a monster being defeated, whoever
finished it; the monster is still on the board here, so its traits can be read.
-}
cardsAboutDefeat :: CardId -> Source -> GameM [Message]
cardsAboutDefeat mid src = do
  invs <- playingInvestigators
  fmap concat $ for invs \i ->
    fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
      b <- assetBehavior cid
      b.afterMonsterDefeated cid i.id mid src

-- | What this investigator's cards add to the result of each die they roll.
dieBonusFor :: TestState -> GameM Int
dieBonusFor ts = do
  i <- getInvestigator ts.investigator
  sum <$> for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    b.dieBonus cid ts.investigator ts

{- | Whether a card of theirs refuses to be discarded, damage and horror included
(Until the End of Time).
-}
cannotBeDiscarded :: CardId -> GameM Bool
cannotBeDiscarded cid = (.undiscardable) <$> assetBehavior cid

{- | Investigators other than this one, in the same space, who may take an engagement
meant for them (Tommy Muldoon).
-}
shieldsFor :: InvestigatorId -> GameM [InvestigatorId]
shieldsFor iid = do
  here <- investigatorSpace iid
  others <- filter ((/= iid) . (.id)) <$> playingInvestigators
  fmap (map (.id))
    $ filterM
      ( \o -> do
          fromCard <- hasAssetWith o.id (.mayTakeEngagement)
          let fromSheet = (investigatorBehavior o.id).mayTakeEngagement
          pure ((fromCard || fromSheet) && isJust here && o.space == here)
      )
      others

-- | Cards its owner may discard to call off an attack, with their names.
attackStoppers :: InvestigatorId -> GameM [(CardId, Text)]
attackStoppers iid = do
  i <- getInvestigator iid
  fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    name <- (.name) <$> getCardDef cid
    pure $ if b.mayStopAttacks then Just (cid, name) else Nothing

-- | The most focus this investigator may put on one skill, which a card may raise.
focusPerSkillFor :: InvestigatorId -> GameM Int
focusPerSkillFor iid = do
  i <- getInvestigator iid
  limits <-
    for [c | c <- i.assets, c `notElem` i.lockedAssets] (fmap (.focusPerSkill) . assetBehavior)
  pure (maximum (1 : limits))

{- | What this investigator's cards offer in place of paying for the card on sale
at that price.
-}
buyOffersFor :: InvestigatorId -> CardId -> Int -> GameM [Reaction]
buyOffersFor iid cid price = do
  i <- getInvestigator iid
  fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \c -> do
    b <- assetBehavior c
    b.buyOffers c iid cid price

{- | What anyone in play offers in place of this monster's activation. Offered to
the table together, since the monster activates once however many could answer.
-}
activationReplacements :: CardId -> GameM [Reaction]
activationReplacements mid = do
  invs <- playingInvestigators
  fmap concat $ for invs \i ->
    fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \c -> do
      b <- assetBehavior c
      b.replacesActivation c i.id mid

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
  fromSheet <- (investigatorBehavior ts.investigator).testOptions ts.investigator ts
  fromMonster <- case ts.kind of
    ActionTest _ (Just mid) -> do
      present <- uses #monsters (Map.member mid)
      if present
        then do
          b <- monsterBehavior mid
          b.testOptions mid ts.investigator ts
        else pure []
    _ -> pure []
  pure (fromCards <> fromSheet <> fromMonster)

{- | A monster's health as it stands: what its card prints, plus its elite health
per investigator, less whatever a card in play takes off it. Never below one, so a
monster is defeated by damage rather than by arithmetic.
-}
effectiveMonsterHealth :: CardId -> GameM (Maybe Int)
effectiveMonsterHealth mid = do
  base <- monsterHealth mid
  b <- monsterBehavior mid
  delta <- b.healthDelta mid
  extra <- codexMonsterHealth mid
  pure (max 1 . (+ (delta + extra)) <$> base)

-- | What the codex says about a monster arriving, or being defeated.
codexAboutMonster
  :: (CodexBehavior -> CodexEntry -> CardId -> GameM [Message]) -> CardId -> GameM [Message]
codexAboutMonster which mid = do
  codex <- use #codex
  fmap concat $ for codex \e -> which (codexBehavior e.number) e mid

-- | Likewise for a defeat, which also carries whatever finished the monster off.
codexAboutDefeat :: CardId -> Source -> GameM [Message]
codexAboutDefeat mid src = do
  codex <- use #codex
  fmap concat $ for codex \e -> (codexBehavior e.number).afterMonsterDefeated e mid src

-- | Health the codex adds to a monster beyond what its own card and behaviour say.
codexMonsterHealth :: CardId -> GameM Int
codexMonsterHealth mid = do
  codex <- use #codex
  sum <$> for codex \e -> (codexBehavior e.number).monsterHealthDelta e mid

{- | What a codex card does instead of putting doom on the scenario sheet; the
first card to answer wins.
-}
sheetDoomInstead :: Int -> GameM (Maybe [Message])
sheetDoomInstead n = do
  codex <- use #codex
  answers <- for codex \e -> (codexBehavior e.number).sheetDoomReplacement e n
  pure (listToMaybe (catMaybes answers))

-- | Cards anyone in play holds that may prevent the damage about to be suffered.
damagePreventionsFor :: HarmPlan -> GameM [(InvestigatorId, Reaction)]
damagePreventionsFor plan = do
  invs <- playingInvestigators
  fmap concat $ for invs \i -> do
    sheet <- (investigatorBehavior i.id).damagePrevention i.id plan
    cards <- fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets, c `notElem` i.usedAssets] \cid -> do
      b <- assetBehavior cid
      b.damagePrevention cid i.id plan
    pure (map (i.id,) (sheet <> cards))

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
