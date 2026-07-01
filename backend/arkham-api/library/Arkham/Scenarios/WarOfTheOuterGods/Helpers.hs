module Arkham.Scenarios.WarOfTheOuterGods.Helpers where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (AgendaAttrs (..), Field (..), onSide)
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Helpers.Scenario (standaloneI18n)
import Arkham.Location.Cards qualified as Locations
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (AgendaAdvancementMethod (..), Message (..), pattern PlaceDoom, pattern RemoveAllDoomFromPlay, pattern RemoveAsset)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "warOfTheOuterGods" a

data Faction = GreenFaction | BlueFaction | RedFaction
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- | Green, then blue, then red. This is also the order of the agenda decks
-- from top to bottom as depicted during setup.
factionOrder :: [Faction]
factionOrder = [GreenFaction, BlueFaction, RedFaction]

agendaDeckN :: Faction -> Int
agendaDeckN = \case
  GreenFaction -> 1
  BlueFaction -> 2
  RedFaction -> 3

deckFaction :: Int -> Maybe Faction
deckFaction = \case
  1 -> Just GreenFaction
  2 -> Just BlueFaction
  3 -> Just RedFaction
  _ -> Nothing

factionAgenda :: Faction -> AgendaMatcher
factionAgenda = AgendaWithDeckId . agendaDeckN

factionEnemyDefs :: Faction -> [CardDef]
factionEnemyDefs = \case
  GreenFaction ->
    [ Enemies.zealotOfParadise
    , Enemies.bringerOfParadise
    , Enemies.bringerOfParadiseWarOfTheOuterGods
    , Enemies.maghanArkat
    , Enemies.vileBroodmaster
    , Enemies.horrificShoggoth
    ]
  BlueFaction ->
    [ Enemies.nihilisticStargazer
    , Enemies.etherealEntity
    , Enemies.etherealEntityWarOfTheOuterGods
    , Enemies.silenus
    , Enemies.theInescapableMaw
    , Enemies.huneStitchedHerald
    ]
  RedFaction ->
    [ Enemies.discipleOfTheSwarm
    , Enemies.trylogog
    , Enemies.trylogogWarOfTheOuterGods
    , Enemies.ezelZenRezl
    , Enemies.droningHorde
    ]

factionTreacheryDefs :: Faction -> [CardDef]
factionTreacheryDefs = \case
  GreenFaction -> [Treacheries.predatorsCall, Treacheries.huntDown]
  BlueFaction -> [Treacheries.deathAndDecay, Treacheries.inevitableEnd]
  RedFaction -> [Treacheries.feastOfLocusts, Treacheries.transmogrify]

-- | All encounter cards belonging to a faction. Used when a faction wins the
-- war and the other factions' cards are removed from the game.
factionEncounterCards :: Faction -> CardMatcher
factionEncounterCards f = mapOneOf cardIs (factionEnemyDefs f <> factionTreacheryDefs f)

factionEnemy :: Faction -> EnemyMatcher
factionEnemy = mapOneOf enemyIs . factionEnemyDefs

-- Defeated faction enemies linger in the entity map placed in 'OutOfPlay
-- RemovedZone'. Enemy queries now default to in-play (non-'OutOfPlay') enemies,
-- so a defeated warring enemy is never offered to move/attack during the enemy
-- phase without any decoration here.
warringEnemy :: EnemyMatcher
warringEnemy = EnemyWithKeyword #warring

getEnemyFaction :: (HasGame m, Tracing m) => EnemyId -> m (Maybe Faction)
getEnemyFaction eid = do
  cardCode <- field EnemyCardCode eid
  pure
    $ find
      (\f -> cardCode `elem` concatMap (.cardCodes) (factionEnemyDefs f))
      factionOrder

-- | Place doom on "the blue agenda" (and friends). If that faction's agenda
-- is not in play (after the war is over) this does nothing.
placeDoomOnFactionAgenda :: (ReverseQueue m, Sourceable source) => source -> Faction -> Int -> m ()
placeDoomOnFactionAgenda source f n =
  selectForMaybeM (factionAgenda f) \agenda -> placeDoom source agenda n

agendaWards :: AgendaAttrs -> Int
agendaWards = countTokens Ward . agendaTokens

-- | When doom would be placed on an agenda, for each ward on that agenda,
-- prevent that amount of doom from being placed. Then remove wards from that
-- agenda equal to the amount of doom prevented.
wardPlaceDoom :: (ReverseQueue m, Sourceable source) => AgendaAttrs -> source -> Int -> m ()
wardPlaceDoom attrs source n = do
  let prevented = min (agendaWards attrs) n
  removeTokens (toSource attrs) (toTarget attrs) Ward prevented
  when (n > prevented) $ push $ PlaceDoom (toSource source) (toTarget attrs) (n - prevented)

-- | Faction agendas have a global doom threshold of 6 (per group, and there
-- is only one group in Single Group Mode). Only doom on the agenda itself
-- counts toward this threshold. When the agenda advances, 6 doom is removed
-- from it and all excess doom moves to the next agenda of that faction.
factionAgendaCheckThreshold :: ReverseQueue m => AgendaAttrs -> m ()
factionAgendaCheckThreshold attrs = when (onSide AS.A attrs) do
  modifiers' <- getModifiers (toTarget attrs)
  let
    applyThresholdModifier acc = \case
      DoomThresholdModifier n -> max 0 (acc + n)
      _ -> acc
    threshold = foldl' applyThresholdModifier 6 modifiers'
  when (attrs.doom >= threshold) do
    checkWhen $ Window.AgendaAdvance attrs.agendaId
    push $ RemoveAllDoomFromPlay $ defaultRemoveDoomMatchers {removeDoomAgendas = AgendaWithId attrs.agendaId}
    push $ AdvanceAgendaBy attrs.agendaId AgendaAdvancedWithDoom
    checkAfter $ Window.AgendaAdvance attrs.agendaId
    let excess = attrs.doom - threshold
    when (excess > 0) do
      push $ ScenarioSpecific "excessDoom" (toJSON (agendaDeckId attrs, excess))

-- | "If this is the first agenda to advance to 1b" (or 2b). True when no
-- other faction's agenda deck has already progressed past that stage.
isFirstAgendaToAdvanceTo :: (HasGame m, Tracing m) => Int -> AgendaAttrs -> m Bool
isFirstAgendaToAdvanceTo n attrs =
  selectNone $ NotAgenda (AgendaWithId attrs.agendaId) <> mapOneOf AgendaWithStep [n + 1 .. 4]

getAgendaStep :: (HasGame m, Tracing m) => AgendaId -> m Int
getAgendaStep = fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep)

-- | The faction that is "in the lead" is the faction that has advanced the
-- farthest through their agenda deck, breaking ties by most doom on their
-- agenda, then by most enemies in play. Multiple factions are returned if
-- the tie cannot be broken.
getLeadFactions :: (HasGame m, Tracing m) => m [Faction]
getLeadFactions = do
  entries <- forMaybeM factionOrder \f -> do
    selectOne (factionAgenda f) >>= traverse \agenda -> do
      step <- getAgendaStep agenda
      doom <- field AgendaDoom agenda
      enemies <- selectCount (factionEnemy f)
      pure (f, (step, doom, enemies))
  let
    best _ [] = []
    best g xs = let m = maximumEx (map (g . snd) xs) in filter ((== m) . g . snd) xs
    byStep = best (\(s, _, _) -> s) entries
    byDoom = best (\(_, d, _) -> d) byStep
    byEnemies = best (\(_, _, e) -> e) byDoom
  pure $ map fst byEnemies

-- | The total doom placed in the windows that triggered a "when any amount
-- of doom is placed on a player card" forced ability.
getPlacedDoomAmount :: [Window.Window] -> Int
getPlacedDoomAmount ws = sum [n | (Window.windowType -> Window.PlacedDoom _ _ n) <- ws]

factionAncientOne :: Faction -> CardDef
factionAncientOne = \case
  GreenFaction -> Enemies.maghanArkat
  BlueFaction -> Enemies.silenus
  RedFaction -> Enemies.ezelZenRezl

factionFinalAgenda :: Faction -> CardDef
factionFinalAgenda = \case
  GreenFaction -> Agendas.theEggHatches
  BlueFaction -> Agendas.silenusDescends
  RedFaction -> Agendas.ezelZenRezlEmerges

-- | "War is Over": flip Hub Dimension to its (Gateway to Destruction) side
-- (putting it into play if needed), spawn the winning faction's Ancient One
-- there, remove the other factions' encounter cards along with each agenda
-- and act from the game, and put the winning faction's set-aside final
-- agenda into play.
warIsOver :: ReverseQueue m => Faction -> m ()
warIsOver winner = do
  hub <-
    selectOne (locationIs Locations.hubDimension) >>= \case
      Just hub -> pure hub
      Nothing -> placeSetAsideLocation Locations.hubDimension
  unsafeReveal hub
  whenJustM (getSetAsideCardMaybe (factionAncientOne winner)) (`createEnemyAt_` hub)
  let losers = filter (/= winner) factionOrder
  for_ losers \f -> do
    push $ RemoveAllCopiesOfEncounterCardFromGame (factionEncounterCards f)
    selectEach (factionEnemy f) removeFromGame
    selectEach (mapOneOf treacheryIs (factionTreacheryDefs f)) removeFromGame
  push $ SetCurrentActDeck 1 []
  for_ losers \f -> push $ SetCurrentAgendaDeck (agendaDeckN f) []
  finalAgenda <- genCard (factionFinalAgenda winner)
  push $ SetCurrentAgendaDeck (agendaDeckN winner) [finalAgenda]

-- | Clues "around" Hub Dimension are not on the location and cannot be
-- discovered by any means.
getCluesAroundHubDimension :: (HasGame m, Tracing m) => m Int
getCluesAroundHubDimension = scenarioCount CluesAroundHubDimension

placeCluesAroundHubDimension :: ReverseQueue m => Int -> m ()
placeCluesAroundHubDimension n = push $ ScenarioCountIncrementBy CluesAroundHubDimension n

removeCluesFromAroundHubDimension :: ReverseQueue m => Int -> m ()
removeCluesFromAroundHubDimension n = push $ ScenarioCountDecrementBy CluesAroundHubDimension n

-- | Place a card facedown underneath an [[Insect]] enemy, as a swarm card.
placeCardAsSwarm :: ReverseQueue m => EnemyId -> Card -> m ()
placeCardAsSwarm insect card = do
  obtainCard card
  push $ PlacedSwarmCard insect card

-- | Place an asset facedown underneath an [[Insect]] enemy, as a swarm card.
placeAssetAsSwarm :: ReverseQueue m => EnemyId -> AssetId -> m ()
placeAssetAsSwarm insect aid = do
  card <- field AssetCard aid
  push $ RemoveAsset aid
  push $ PlacedSwarmCard insect card

placeMutations :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> Int -> m ()
placeMutations source eid n = placeTokens source (EnemyTarget eid) Mutation n

removeMutations :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> Int -> m ()
removeMutations source eid n = removeTokens (toSource source) (EnemyTarget eid) Mutation n

getMutations :: (HasGame m, Tracing m) => EnemyId -> m Int
getMutations = fieldMap EnemyTokens (countTokens Mutation)

-- | Order enemies green, then blue, then red. Enemies without a faction are
-- dropped.
sortEnemiesByFaction :: (HasGame m, Tracing m) => [EnemyId] -> m [EnemyId]
sortEnemiesByFaction eids = do
  withFaction <- for eids \eid -> (,eid) <$> getEnemyFaction eid
  pure [eid | f <- factionOrder, (mf, eid) <- withFaction, mf == Just f]

-- | The warring enemies that would move during the "hunter enemies move"
-- step: ready, unengaged, not already at a location with an enemy of a
-- different faction, and with some warring enemy of a different faction to
-- move toward.
getWarringMovers :: (HasGame m, Tracing m) => m [EnemyId]
getWarringMovers = do
  warring <- select $ warringEnemy <> ReadyEnemy <> UnengagedEnemy
  flip filterM warring \enemy ->
    getEnemyFaction enemy >>= \case
      Nothing -> pure False
      Just f -> do
        colocated <-
          select
            $ EnemyAt (locationWithEnemy enemy)
            <> mapOneOf factionEnemy (filter (/= f) factionOrder)
        if notNull colocated
          then pure False
          else selectAny $ warringEnemy <> not_ (factionEnemy f) <> not_ (EnemyWithId enemy)

-- | The warring enemies that would attack during the "resolve enemy
-- attacks" step: ready, unengaged, and at a location with a warring enemy
-- of a different faction.
getWarringAttackers :: (HasGame m, Tracing m) => m [EnemyId]
getWarringAttackers = do
  warring <- select $ warringEnemy <> ReadyEnemy <> UnengagedEnemy
  filterM (fmap notNull . getWarringTargets) warring

-- | The warring enemies of a different faction at the given enemy's location.
getWarringTargets :: (HasGame m, Tracing m) => EnemyId -> m [EnemyId]
getWarringTargets eid =
  getEnemyFaction eid >>= \case
    Nothing -> pure []
    Just f ->
      select
        $ warringEnemy
        <> not_ (factionEnemy f)
        <> EnemyAt (locationWithEnemy eid)
        <> not_ (EnemyWithId eid)

-- | While The Inescapable Maw is in play, anytime an investigator is forced
-- to "decide" by an ability on a blue encounter card, they must choose two
-- options instead of one.
blueDecide :: ReverseQueue m => InvestigatorId -> ChooseT m () -> m ()
blueDecide iid options = do
  maw <- selectAny $ enemyIs Enemies.theInescapableMaw
  if maw then chooseNM iid 2 options else chooseOneM iid options
