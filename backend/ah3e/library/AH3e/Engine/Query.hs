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
  pure $ Map.findWithDefault 0 skill d.skills + Map.findWithDefault 0 skill i.focus

focusCount :: Investigator -> Int
focusCount i = sum (Map.elems i.focus)

-- | The sheet's focus limit plus anything the investigator holds that raises it (The Moon).
focusLimit :: InvestigatorId -> GameM (Maybe Int)
focusLimit iid = do
  base <- (.focusLimit) <$> getInvestigatorDef iid
  i <- getInvestigator iid
  bonus <- sum <$> for i.assets (fmap focusLimitBonus . cardCode)
  pure ((+ bonus) <$> base)

investigatorHealth :: InvestigatorId -> GameM Int
investigatorHealth iid = (.health) <$> getInvestigatorDef iid

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
unstableSpaces :: GameM [SpaceId]
unstableSpaces = do
  discard <- use (#decks . #eventDiscard)
  case discard of
    (top : _) ->
      getCardDef top <&> \d -> case d.kind of
        EventCard e -> nub e.doomSpaces
        _ -> []
    [] -> pure . (.startingSpace) <$> getScenarioDef

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

ruleSpaces :: Maybe CardId -> SpaceRule -> GameM [SpaceId]
ruleSpaces mid = \case
  UnstableSpace -> unstableSpaces
  MostDoomSpace -> mostDoomSpaces
  StartingSpace -> pure . (.startingSpace) <$> getScenarioDef
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
    SpendMoney n -> pure (i.money >= n)
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
        EachInvestigatorInYourSpace -> here
        InvestigatorInYourSpace -> here
        InvestigatorOrAllyInYourSpace -> here
      owners = case r of
        YouOrAlly -> [self]
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

-- | Whether this card's once-per-round ability has already been spent.
usedThisRound :: CardId -> InvestigatorId -> GameM Bool
usedThisRound cid iid = elem cid . (.usedAssets) <$> getInvestigator iid

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
