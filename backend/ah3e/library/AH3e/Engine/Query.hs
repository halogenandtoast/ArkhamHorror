module AH3e.Engine.Query where

import AH3e.Content
import AH3e.Engine.Monad
import AH3e.Game
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

getCardDef :: HasCallStack => CardId -> GameM CardDef
getCardDef cid = do
  code <- cardCode cid
  pure $ fromJustNote ("no card def for " <> show code) (cardDef code)

getScenarioDef :: GameM ScenarioDef
getScenarioDef = do
  code <- fromJustNote "no scenario chosen" <$> use #scenario
  pure $ fromJustNote ("no scenario def for " <> show code) (scenarioDef code)

getInvestigatorDef :: HasCallStack => InvestigatorId -> GameM InvestigatorDef
getInvestigatorDef iid = pure $ fromJustNote ("no investigator def for " <> show iid) (investigatorDef iid)

monsterDef :: CardId -> GameM MonsterDef
monsterDef mid =
  getCardDef mid <&> \d -> case d.kind of
    MonsterCard m -> m
    _ -> error ("not a monster " <> show mid)

assetDef :: CardId -> GameM (Maybe AssetDef)
assetDef cid =
  getCardDef cid <&> \d -> case d.kind of
    AssetCard a -> Just a
    _ -> Nothing

isPlaying :: Investigator -> Bool
isPlaying i = i.status == Playing && isJust i.space

playingInvestigators :: GameM [Investigator]
playingInvestigators = filter isPlaying <$> investigatorsInPlay

investigatorIsPlaying :: InvestigatorId -> GameM Bool
investigatorIsPlaying iid = uses #investigators (maybe False isPlaying . Map.lookup iid)

getSpace :: HasCallStack => SpaceId -> GameM Space
getSpace sid = uses (#board . #spaces) (fromJustNote ("unknown space " <> show sid) . Map.lookup sid)

getNeighborhood :: HasCallStack => NeighborhoodId -> GameM Neighborhood
getNeighborhood nid =
  uses (#board . #neighborhoods) (fromJustNote ("unknown neighborhood " <> show nid) . Map.lookup nid)

spaceL :: SpaceId -> Lens' Game Space
spaceL sid = singular (#board . #spaces . ix sid)

neighborhoodL :: NeighborhoodId -> Lens' Game Neighborhood
neighborhoodL nid = singular (#board . #neighborhoods . ix nid)

investigatorSpace :: InvestigatorId -> GameM (Maybe SpaceId)
investigatorSpace iid = (.space) <$> getInvestigator iid

investigatorNeighborhood :: InvestigatorId -> GameM (Maybe NeighborhoodId)
investigatorNeighborhood iid = do
  msid <- investigatorSpace iid
  board <- use #board
  pure $ msid >>= (`spaceNeighborhood` board)

{- | The town an investigator stands in. A street belongs to no neighborhood and
so to no town, which is what keeps a card printed "in Kingsport" off one.
-}
investigatorTown :: InvestigatorId -> GameM (Maybe Town)
investigatorTown iid = investigatorNeighborhood iid >>= traverse (fmap (.town) . getNeighborhood)

investigatorsAt :: SpaceId -> GameM [Investigator]
investigatorsAt sid = filter ((== Just sid) . (.space)) <$> playingInvestigators

monstersAt :: SpaceId -> GameM [Monster]
monstersAt sid = uses #monsters (filter ((== sid) . (.space)) . Map.elems)

engagedMonsters :: InvestigatorId -> GameM [Monster]
engagedMonsters iid = uses #monsters (filter (isEngagedWith iid) . Map.elems)

isEngagedWith :: InvestigatorId -> Monster -> Bool
isEngagedWith iid m = case m.state of
  Engaged is -> iid `elem` is
  _ -> False

hasKeyword :: Keyword -> CardId -> GameM Bool
hasKeyword k mid = elem k . (.keywords) <$> monsterDef mid

-- rules 428.5, 495.2-495.3
isRestrictedByEngagement :: InvestigatorId -> GameM Bool
isRestrictedByEngagement iid = do
  ms <- engagedMonsters iid
  anyM (fmap not . hasKeyword Watcher . (.card)) ms

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = foldr (\x acc -> p x >>= \b -> if b then pure True else acc) (pure False)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = fmap not . anyM (fmap not . p)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' = filterM

skillValue :: InvestigatorId -> Skill -> GameM Int
skillValue iid skill = do
  i <- getInvestigator iid
  d <- getInvestigatorDef iid
  shared <- sharedFocus iid skill
  -- a card may raise every skill for a phase (Adventurous Spirit)
  phase <- use #phase
  codes <- traverse cardCode [c | c <- i.assets, c `notElem` i.lockedAssets]
  let phasely = if phase == EncounterPhase then sum (map encounterPhaseSkillBonus codes) else 0
  pure
    $ Map.findWithDefault 0 skill d.skills
    + Map.findWithDefault 0 skill i.focus
    + shared
    + phasely

{- | Money on the cards this investigator holds that they may spend as their own
(Calling in Favors).
-}
cardMoney :: InvestigatorId -> GameM Int
cardMoney iid = do
  held <- spendableMoneyHeld iid
  pure (sum (map snd held))

-- | Everything they could put towards a price: their own money and their cards'.
availableMoney :: InvestigatorId -> GameM Int
availableMoney iid = do
  i <- getInvestigator iid
  (i.money +) <$> cardMoney iid

{- | Spend that much, their own money first and then whatever their cards are
holding for them, so an ordinary purchase asks nothing extra.
-}
spendMoney :: InvestigatorId -> Int -> GameM ()
spendMoney iid n = do
  i <- getInvestigator iid
  let fromPocket = min n i.money
  investigatorL iid . #money %= max 0 . subtract fromPocket
  held <- spendableMoneyHeld iid
  go (n - fromPocket) held
 where
  go left [] = when (left > 0) (logText "Not enough money")
  go left ((cid, have) : rest)
    | left <= 0 = pure ()
    | otherwise = do
        let taken = min left have
        assetL cid . #tokens . at "money" ?= have - taken
        go (left - taken) rest

-- | The cards holding money for them, with how much each holds.
spendableMoneyHeld :: InvestigatorId -> GameM [(CardId, Int)]
spendableMoneyHeld iid = do
  i <- getInvestigator iid
  fmap catMaybes $ for i.assets \cid -> do
    code <- cardCode cid
    held <- uses #assets (maybe 0 (Map.findWithDefault 0 "money" . (.tokens)) . Map.lookup cid)
    pure $ if code `elem` spendableMoneyCards && held > 0 then Just (cid, held) else Nothing

{- | What the others in this space lend them: a card may share each skill its holder
has focused with everyone standing there (Synergy).
-}
sharedFocus :: InvestigatorId -> Skill -> GameM Int
sharedFocus iid skill = do
  here <- investigatorSpace iid
  others <- filter ((/= iid) . (.id)) <$> playingInvestigators
  shares <- for [o | o <- others, isJust here, o.space == here] \o -> do
    codes <- traverse cardCode [c | c <- o.assets, c `notElem` o.lockedAssets]
    let sharing = any (`elem` sharesFocusedSkills) codes
    pure (if sharing && Map.findWithDefault 0 skill o.focus > 0 then 1 else 0)
  pure (sum shares)

focusCount :: Investigator -> Int
focusCount i = sum (Map.elems i.focus)

-- | The sheet's focus limit plus anything the investigator holds that raises it (The Moon).
focusLimit :: InvestigatorId -> GameM (Maybe Int)
focusLimit iid = do
  base <- (.focusLimit) <$> getInvestigatorDef iid
  i <- getInvestigator iid
  -- a double-sided card only raises the limit on the side that prints it (DRIVEN)
  bonus <- fmap sum $ for i.assets \cid -> do
    flipped <- uses #assets (maybe False (.flipped) . Map.lookup cid)
    if flipped then pure 0 else focusLimitBonus <$> cardCode cid
  -- a sheet may say its limit is counted rather than printed (Dexter Drake's
  -- spells, Charlie Kane's allies)
  counted <-
    if
      | iid `elem` focusLimitFromSpells -> Just . length <$> matchingAssets iid SpellCard
      | iid `elem` focusLimitFromAllies -> Just . length <$> matchingAssets iid AllyCard
      | otherwise -> pure Nothing
  pure ((+ bonus) <$> maybe base Just counted)

-- | Whether that rumor headline is the one sitting in the codex.
rumorInPlay :: CardCode -> GameM Bool
rumorInPlay code = do
  mr <- use #rumor
  codes <- traverse (cardCode . (.card)) mr
  pure (codes == Just code)

{- | Printed health, less what a rumor has taken off it. Piscine Pox reduces
every investigator's health while it is in the codex; the floor keeps a card from
reducing anyone to nothing.
-}
investigatorHealth :: InvestigatorId -> GameM Int
investigatorHealth iid = do
  base <- (.health) <$> getInvestigatorDef iid
  plague <- rumorInPlay "piscine-pox-paralyzes-port"
  pure (max 1 (base - (if plague then 1 else 0)))

investigatorSanity :: InvestigatorId -> GameM Int
investigatorSanity iid = (.sanity) <$> getInvestigatorDef iid

monsterHealth :: CardId -> GameM (Maybe Int)
monsterHealth mid = do
  d <- monsterDef mid
  m <- getMonster mid
  n <- use #startingInvestigatorCount
  pure
    $ if Shrouded `elem` d.keywords && m.state == Ready
      then Nothing
      else Just (d.health + d.elite * n)

hasCondition :: InvestigatorId -> ConditionName -> GameM Bool
hasCondition iid name = elem name <$> conditionNames iid

conditionNames :: InvestigatorId -> GameM [ConditionName]
conditionNames iid = do
  i <- getInvestigator iid
  fmap catMaybes $ for i.assets \cid -> do
    d <- getCardDef cid
    a <- use (assetL cid)
    pure case d.kind of
      ConditionCard c -> Just (if a.flipped then c.back.name else c.front.name)
      _ -> Nothing

-- | The card of a condition the investigator has, matched on its face-up name.
conditionCard :: InvestigatorId -> ConditionName -> GameM (Maybe CardId)
conditionCard iid name = do
  i <- getInvestigator iid
  matches <- filterM' isNamed i.assets
  pure (listToMaybe matches)
 where
  isNamed cid = do
    d <- getCardDef cid
    a <- use (assetL cid)
    pure case d.kind of
      ConditionCard c -> (if a.flipped then c.back.name else c.front.name) == name
      _ -> False

codexHas :: ArchiveNumber -> GameM Bool
codexHas n = uses #codex (any ((== n) . (.number)))

-- rule 493

{- | The space a sheet starts its investigators in, as long as it is still on the
board: a scenario can name a space its own map does not have, and one that devours
spaces can eat the space out from under the sheet. Everything that reads the starting
space goes through here, because a SpaceId the board does not know is an error
wherever it is used.
-}
startingSpaceOnBoard :: GameM (Maybe SpaceId)
startingSpaceOnBoard = do
  start <- (.startingSpace) <$> getScenarioDef
  spaces <- uses (#board . #spaces) Map.keys
  pure (if start `elem` spaces then Just start else listToMaybe spaces)

unstableSpaces :: GameM [SpaceId]
unstableSpaces =
  use #unstableSpace >>= \case
    Just sid -> pure [sid]
    Nothing -> printedUnstableSpaces

{- | Where the event deck says the unstable space is, whatever a card has to say. A
scenario may take a whole tile off the board -- Tsathoggua eats one -- while the card on
the discard still names a space that stood on it, so what it names is only the unstable
space while it is still there.
-}
printedUnstableSpaces :: GameM [SpaceId]
printedUnstableSpaces = do
  discard <- use (#decks . #eventDiscard)
  named <- case discard of
    (top : _) ->
      getCardDef top <&> \d -> case d.kind of
        EventCard e -> nub e.doomSpaces
        _ -> []
    [] -> pure []
  board <- use #board
  case filter (`Map.member` board.spaces) named of
    [] -> maybeToList <$> startingSpaceOnBoard
    there -> pure there

mostDoomSpaces :: GameM [SpaceId]
mostDoomSpaces = do
  spaces <- uses (#board . #spaces) (filter (isNeighborhoodSpace . (.kind)) . Map.elems)
  let best = maximum (0 : map (.doom) spaces)
  pure [s.id | s <- spaces, s.doom == best]

ruleInvestigators :: InvestigatorRule -> GameM [Investigator]
ruleInvestigators rule = do
  invs <- playingInvestigators
  case rule of
    LowestSkill sk -> extremal minimum (\i -> skillValue i.id sk) invs
    HighestSkill sk -> extremal maximum (\i -> skillValue i.id sk) invs
    MostClues -> extremal maximum (pure . (.clues)) invs
    FewestClues -> extremal minimum (pure . (.clues)) invs
    MostMoney -> extremal maximum (pure . (.money)) invs
    MostRemnants -> extremal maximum (pure . (.remnants)) invs
    MostSpells -> extremal maximum (\i -> length <$> filterM' (cardMatches SpellCard) i.assets) invs
    MostAllies -> extremal maximum (\i -> length <$> filterM' (cardMatches AllyCard) i.assets) invs
    MostDamage -> extremal maximum (pure . (.damage)) invs
    LeastDamage -> extremal minimum (pure . (.damage)) invs
    MostItems -> extremal maximum (\i -> length <$> filterM' (cardMatches ItemCard) i.assets) invs
    NearestInvestigator -> pure invs
    NamedInvestigator who -> pure (filter ((== who) . (.id)) invs)
    LowestRemainingHealth -> extremal minimum (\i -> subtract i.damage <$> investigatorHealth i.id) invs
    LowestRemainingSanity -> extremal minimum (\i -> subtract i.horror <$> investigatorSanity i.id) invs
    TheLeader -> do
      l <- leaderPlayer
      pure (filter ((== l) . (.player)) invs)
    CustomInvestigatorRule _ -> pure []
 where
  extremal pick f xs = do
    scored <- for xs \x -> (x,) <$> f x
    case scored of
      [] -> pure []
      _ -> do
        let best = pick (map snd scored)
        pure [x | (x, v) <- scored, v == best]

{- | The spaces a rule names, less any a scenario has taken off the board -- an
unstable space or a starting space can be devoured like any other.
-}

{- | Whether exhausting this monster would do anything: 443.4 and the Massive,
Relentless and Shrouded keywords all refuse it, and an exhausted one is already
there. Cards that pay a cost to exhaust read this first, so the cost is never
spent on a no-op -- and so a ready Shrouded monster is not named in a prompt.
-}
canBeExhausted :: CardId -> GameM Bool
canBeExhausted mid = do
  d <- monsterDef mid
  m <- uses #monsters (Map.lookup mid)
  let refuses =
        any (`elem` d.keywords) [Massive, Relentless]
          || (Shrouded `elem` d.keywords && maybe False ((== Ready) . (.state)) m)
  pure (isJust m && not refuses && maybe False ((/= Exhausted) . (.state)) m)

ruleSpaces :: Maybe CardId -> SpaceRule -> GameM [SpaceId]
ruleSpaces mid rule = do
  onBoard <- uses (#board . #spaces) Map.keysSet
  filter (`Set.member` onBoard) <$> ruleSpaces' mid rule

ruleSpaces' :: Maybe CardId -> SpaceRule -> GameM [SpaceId]
ruleSpaces' mid = \case
  UnstableSpace -> unstableSpaces
  MostDoomSpace -> mostDoomSpaces
  StartingSpace -> maybeToList <$> startingSpaceOnBoard
  NamedSpace sid -> pure [sid]
  PreySpace rule -> mapMaybe (.space) <$> ruleInvestigators rule
  NearestStreetTo mrule -> do
    streets <- uses (#board . #spaces) (map (.id) . filter (isStreetLike . (.kind)) . Map.elems)
    rule <- case mrule of
      Just r -> pure (Just r)
      Nothing -> maybe (pure Nothing) (fmap preyRule . monsterDef) mid
    froms <- maybe (pure []) (fmap (mapMaybe (.space)) . ruleInvestigators) rule
    case froms of
      [] -> pure streets
      _ -> nub . concat <$> traverse (`closestTo` streets) froms
  CustomSpaceRule _ -> pure []

preyRule :: MonsterDef -> Maybe InvestigatorRule
preyRule d = case d.activation of
  Hunter r -> Just r
  Patrol _ r -> r
  _ -> Nothing

-- rule 202.2g: the option closest to the monster takes precedence
closestTo :: SpaceId -> [SpaceId] -> GameM [SpaceId]
closestTo from targets = do
  board <- use #board
  let dist = distancesFrom (`monsterAdjacent` board) from
      scored = [(t, d) | t <- targets, Just d <- [Map.lookup t dist]]
  pure $ case scored of
    [] -> []
    _ -> let best = minimum (map snd scored) in [t | (t, d) <- scored, d == best]

nextStepsToward :: SpaceId -> SpaceId -> GameM [SpaceId]
nextStepsToward from target = do
  board <- use #board
  let dist = distancesFrom (`monsterAdjacent` board) target
  pure case Map.lookup from dist of
    Just d | d > 0 -> [n | n <- monsterAdjacent from board, Map.lookup n dist == Just (d - 1)]
    _ -> []

canPayCost :: InvestigatorId -> Cost -> GameM Bool
canPayCost iid cost = do
  i <- getInvestigator iid
  case cost of
    SpendMoney n -> (>= n) <$> availableMoney i.id
    SpendRemnants n -> pure (i.remnants >= n)
    SpendClues n -> pure (i.clues >= n)
    SpendFocus n -> pure (focusCount i >= n)
    CostDamage _ -> pure True
    CostHorror _ -> pure True
    CostDelayed -> pure (not i.delayed)
    CostCondition name -> not <$> hasCondition iid name
    CostDiscard f -> not . null <$> matchingAssets iid f
    AllOf cs -> allM (canPayCost iid) cs

matchingAssets :: InvestigatorId -> CardFilter -> GameM [CardId]
matchingAssets iid f = do
  i <- getInvestigator iid
  filterM (cardMatches f) i.assets

cardMatches :: CardFilter -> CardId -> GameM Bool
cardMatches f cid = do
  d <- getCardDef cid
  let assetTy = case d.kind of
        AssetCard a -> Just a
        _ -> Nothing
  pure case f of
    AnyCard -> True
    WithTrait t -> maybe False (elem t . (.traits)) assetTy
    NamedCard n -> T.toCaseFold n == T.toCaseFold d.name
    ItemCard -> ((.assetType) <$> assetTy) == Just Item
    AllyCard -> ((.assetType) <$> assetTy) == Just Ally
    SpellCard -> ((.assetType) <$> assetTy) == Just Spell

{- | Who a recovery could reach that has something to recover: the investigators
and allies with damage (when recovering health) or horror (when recovering sanity).
-}
recoverTargets :: EffectCtx -> Recipient -> Int -> Int -> GameM ([InvestigatorId], [CardId])
recoverTargets ctx r hp sp = do
  let iid = ctx.investigator
  sid <- investigatorSpace iid
  here <- maybe (pure []) investigatorsAt sid
  self <- getInvestigator iid
  let reachable = case r of
        You -> [self]
        YouOrAlly -> [self]
        YouAndYourAllies -> [self]
        EachInvestigatorInYourSpace -> here
        InvestigatorInYourSpace -> here
        InvestigatorOrAllyInYourSpace -> here
      owners = case r of
        YouOrAlly -> [self]
        YouAndYourAllies -> [self]
        InvestigatorOrAllyInYourSpace -> here
        _ -> []
      hurt d h = (hp > 0 && d > 0) || (sp > 0 && h > 0)
  allies <- filterM (cardMatches AllyCard) (concatMap (.assets) owners)
  hurtAllies <-
    filterM (\c -> maybe False (\a -> hurt a.damage a.horror) <$> use (#assets . at c)) allies
  pure ([i.id | i <- reachable, hurt i.damage i.horror], hurtAllies)

{- | Whether offering this effect as a choice could change anything: a recovery
nobody in reach needs, or a focus with every allowed skill already focused or
the focus limit already reached, is not worth offering.
-}

-- | Whether an ability of the investigator's own has been spent this round.
usedAbility :: InvestigatorId -> Text -> GameM Bool
usedAbility iid key = elem key . (.usedAbilities) <$> getInvestigator iid

-- | Whether this investigator holds a particular card, which cards name each other by.
holdsCard :: InvestigatorId -> CardCode -> GameM Bool
holdsCard iid wanted = do
  i <- getInvestigator iid
  anyM (fmap (== wanted) . cardCode) [c | c <- i.assets, c `notElem` i.lockedAssets]

-- | Whether this card's once-per-round ability has already been spent.
usedThisRound :: CardId -> InvestigatorId -> GameM Bool
usedThisRound cid iid = elem cid . (.usedAssets) <$> getInvestigator iid

{- | Whether nobody else stands anywhere in this investigator's neighborhood. A
street is in no neighborhood, so nobody is ever alone in one.
-}
onlyInvestigatorInNeighborhood :: InvestigatorId -> GameM Bool
onlyInvestigatorInNeighborhood iid =
  investigatorNeighborhood iid >>= \case
    Nothing -> pure False
    Just nid -> do
      spaces <- uses #board (neighborhoodSpaces nid)
      others <- filter ((/= iid) . (.id)) <$> playingInvestigators
      pure (not (any (maybe False (`elem` spaces) . (.space)) others))

-- | Everyone in play that no monster is engaged with.
unengagedInvestigators :: GameM [Investigator]
unengagedInvestigators = playingInvestigators >>= filterM (fmap null . engagedMonsters . (.id))

-- | Clues sitting in the investigator's neighborhood; zero while in a street.
neighborhoodClues :: InvestigatorId -> GameM Int
neighborhoodClues iid =
  investigatorNeighborhood iid >>= maybe (pure 0) (fmap (.clues) . getNeighborhood)

effectUseful :: EffectCtx -> Effect -> GameM Bool
effectUseful ctx = \case
  RecoverHealth r _ -> needs r 1 0
  RecoverSanity r _ -> needs r 0 1
  RecoverBoth r _ _ -> needs r 1 1
  Focus mskill evenIfExceeds -> do
    i <- getInvestigator ctx.investigator
    limit <- focusLimit ctx.investigator
    let open = [s | s <- maybe allSkills pure mskill, Map.findWithDefault 0 s i.focus == 0]
        roomLeft = evenIfExceeds || maybe True (focusCount i <) limit
    pure (not (null open) && roomLeft)
  Choose options -> anyM (effectUseful ctx . snd) options
  _ -> pure True
 where
  needs r hp sp = (\(is, as) -> not (null is && null as)) <$> recoverTargets ctx r hp sp
