module Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers where

import Arkham.Ability
import Arkham.Attack (enemyAttack)
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push, removeAllMessagesMatching)
import Arkham.Classes.Query
import Arkham.Constants (pattern AbilityAttack, pattern AbilityEvade)
import Arkham.Draw.Types (newCardDraw)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log (scenarioCount, scenarioCountIncrementBy)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher
import Arkham.Message (
  GroupKey (HunterGroup),
  Message (DrawCards, HandleGroupTarget, ScenarioCountSet, ScenarioSpecific, StoryMessage),
  pattern EnemyWillAttack,
  pattern PatrolMove,
 )
import Arkham.Message.Lifted (createEnemyAt_)
import Arkham.Message.Lifted.Card (obtainCard)
import Arkham.Message.Lifted.Queue
import Arkham.Message.Story (StoryMessage (RemoveStory))
import Arkham.Modifier (Modifier)
import Arkham.Prelude
import Arkham.Scenario.Deck (ScenarioDeckKey (CthulhuDeck))
import Arkham.ScenarioLogKey (ScenarioCountKey (CthulhuRage))
import Arkham.Source (Source (EnemySource), Sourceable)
import Arkham.Story.Types (StoryAttrs)
import Arkham.Target (Target (AbilityTarget, EnemyTarget))
import Arkham.Tracing (Tracing)
import Arkham.Trait (Trait (Rooftop))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Map.Monoidal.Strict (MonoidalMap)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theDoomOfArkhamPartII" a

{- | The shared, unscoped labels ("take 1 damage", "discard an asset you control")
that the Cthulhu deck action cards reuse. 'unscoped' resets an existing scope rather
than establishing one, and the action cards open no scope of their own, so it needs
one to reset from.
-}
sharedI18n :: (HasI18n => a) -> a
sharedI18n a = scenarioI18n (unscoped a)

{- | "The number of resources under 'Cthulhu's Rage' on the scenario reference card
indicate Cthulhu's anger toward the investigators." Rage has no inherent effect,
but the skull token scales with it and nearly every action card tests against it,
so it is a first-class scenario count, like the Depths of Yoth's depth.
-}
getCthulhuRage :: (HasGame m, Tracing m) => m Int
getCthulhuRage = scenarioCount CthulhuRage

increaseCthulhuRage :: ReverseQueue m => Int -> m ()
increaseCthulhuRage = scenarioCountIncrementBy CthulhuRage

-- | "Set Cthulhu's Rage to 5 (4 instead if there are 1 or 2 investigators.)"
setCthulhuRage :: ReverseQueue m => Int -> m ()
setCthulhuRage = push . ScenarioCountSet CthulhuRage

{- | The three double-sided enemy cards that live on the Cthulhu Board, in board
order. The slot is fixed by the card, so "return it to its place on the Cthulhu
Board" needs no stored state.
-}
cthulhuBoardSlots :: [(Int, CthulhuFacet)]
cthulhuBoardSlots =
  [ (1, HoaryWings)
  , (2, FierceVisage)
  , (3, WickedClaw)
  ]

{- | Each Cthulhu facet paired with its @Enraged@ face. Banish Him! both returns
banished facets to the board non-Enraged and later flips every one of them Enraged,
so it needs the mapping in both directions.
-}
cthulhuFacets :: [(CardDef, CardDef)]
cthulhuFacets =
  [ (Enemies.cthulhuHoaryWings, Enemies.cthulhuHoaryWingsEnraged)
  , (Enemies.cthulhuFierceVisage, Enemies.cthulhuFierceVisageEnraged)
  , (Enemies.cthulhuWickedClaw, Enemies.cthulhuWickedClawEnraged)
  ]

{- | Matches a Cthulhu facet on either of its faces: a facet flipped to its
@Enraged@ side is a different card code but the same card.
-}
cthulhuFacet :: CthulhuFacet -> EnemyMatcher
cthulhuFacet (toCardDef -> def) = mapOneOf enemyIs def.defs

data CthulhuFacet = HoaryWings | FierceVisage | WickedClaw

cthulhuFacetName :: CthulhuFacet -> Text
cthulhuFacetName = \case
  HoaryWings -> "hoaryWings"
  FierceVisage -> "fierceVisage"
  WickedClaw -> "wickedClaw"

highlightCthulhuFacet :: ReverseQueue m => CthulhuFacet -> m () -> m ()
highlightCthulhuFacet facet body = do
  push $ ScenarioSpecific "setCthulhuActiveFacet" (toJSON $ Just $ cthulhuFacetName facet)
  body
  push $ ScenarioSpecific "setCthulhuActiveFacet" (toJSON (Nothing :: Maybe Text))

instance HasCardDef CthulhuFacet where
  toCardDef = \case
    HoaryWings -> Enemies.cthulhuHoaryWings
    FierceVisage -> Enemies.cthulhuFierceVisage
    WickedClaw -> Enemies.cthulhuWickedClaw

whenCthulhuHas :: ReverseQueue m => CthulhuFacet -> m () -> m ()
whenCthulhuHas facet body = whenAny (cthulhuFacet facet) do
  highlightCthulhuFacet facet body

{- | The facets still on the board — in play, rather than banished to the victory
display. Cthulhu's combined enemy-phase attack sums only these.
-}
getCthulhuBoardEnemies :: (HasGame m, Tracing m) => m [EnemyId]
getCthulhuBoardEnemies = select $ mapOneOf (cthulhuFacet . snd) cthulhuBoardSlots

{- | Cthulhu's location. Almost every Cthulhu deck action card is written relative
to it ("each investigator at Cthulhu's location", "the nearest investigator to
Cthulhu"), so the lookup lives here rather than in eleven card modules.
-}
getCthulhuLocation :: (HasGame m, Tracing m) => m (Maybe LocationId)
getCthulhuLocation = selectOne $ LocationWithEnemy (enemyIs Enemies.cthulhuAncientEvil)

getInvestigatorsWithCthulhu :: (HasGame m, Tracing m) => m [InvestigatorId]
getInvestigatorsWithCthulhu = getCthulhuLocation >>= maybe (pure []) (select . investigatorAt)

eachInvestigatorWithCthulhu :: (HasGame m, Tracing m) => (InvestigatorId -> m ()) -> m ()
eachInvestigatorWithCthulhu f = getInvestigatorsWithCthulhu >>= traverse_ f

withCthulhuLocation :: (HasGame m, Tracing m) => (LocationId -> m ()) -> m ()
withCthulhuLocation = whenJustM getCthulhuLocation

-- | The in-play facet matching a definition, on whichever face it is currently showing.
getCthulhuFacet :: (HasGame m, Tracing m) => CthulhuFacet -> m (Maybe EnemyId)
getCthulhuFacet = selectOne . cthulhuFacet

{- | "Cthulhu /(X)/ attacks <investigator>." A no-op when that facet has been
banished to the victory display, which is what "if no attack was made" turns on.
-}
cthulhuFacetAttacks
  :: (ReverseQueue m, Sourceable source) => source -> CthulhuFacet -> InvestigatorId -> m Bool
cthulhuFacetAttacks source facet iid =
  getCthulhuFacet facet >>= \case
    Nothing -> pure False
    Just eid -> do
      highlightCthulhuFacet facet do
        push $ EnemyWillAttack $ enemyAttack eid source iid
      pure True

{- | "Resolve Cthulhu's patrol keyword an additional time." (Hurricane Force, Dire
Gale.)

Mirrors what the enemy runner does for the printed keyword during 'HuntersMove',
including its "wants to patrol" guard, so an extra patrol behaves exactly like the
scheduled one — an enemy that is engaged, or already standing somewhere its patrol
matcher accepts, stays put.
-}
resolveCthulhuPatrol :: ReverseQueue m => m ()
resolveCthulhuPatrol =
  selectOne (enemyIs Enemies.cthulhuAncientEvil) >>= traverse_ \eid -> do
    let lMatcher = CanHaveFloodLevelIncreased
    wantsToPatrol <- eid <=~> (UnengagedEnemy <> not_ (EnemyAt $ replaceThatEnemy eid lMatcher))
    when wantsToPatrol
      $ push
      $ HandleGroupTarget HunterGroup (EnemyTarget eid) [PatrolMove eid $ replaceThatEnemy eid lMatcher]

{- | "If Cthulhu did not move via his patrol keyword this round" (Dire Gale).

Enemy movement is not in the per-investigator history, so Cthulhu records his own
patrol as a round-scoped modifier on himself — round modifiers expire exactly when
"this round" does.
-}
cthulhuPatrolledMarker :: ModifierType
cthulhuPatrolledMarker = ScenarioModifier "cthulhuPatrolled"

cthulhuPatrolledThisRound :: (HasGame m, Tracing m) => m Bool
cthulhuPatrolledThisRound =
  selectAny $ enemyIs Enemies.cthulhuAncientEvil <> EnemyWithModifier cthulhuPatrolledMarker

{- | Cancel the automatic trip to the Cthulhu discard pile that the scenario queues
for every drawn action card.

Three cards dispose of themselves instead: Fifth Eye and Hope Fades put themselves
into play next to the agenda deck, and Demolition shuffles itself back in with the
rest of the discard pile. Each resolves before the queued discard, so it can lift
that message back out of the real queue.
-}
retainCthulhuCard :: (MonadTrans t, HasQueue Message m) => Card -> t m ()
retainCthulhuCard card = lift $ removeAllMessagesMatching \case
  ScenarioSpecific "discardCthulhuCard" v -> v == toJSON card
  _ -> False

{- | "Discard this card", for the action cards that put themselves into play next to
the agenda deck (Fifth Eye, Hope Fades).

Their automatic discard was cancelled by 'retainCthulhuCard' when they entered play,
so discarding now has to do both halves by hand: take the story off the table and
file its card into the Cthulhu discard pile. It cannot go through 'toDiscard' —
nothing handles @Discard@ for a @StoryTarget@, so that call silently does nothing.
-}
discardCthulhuCard :: ReverseQueue m => StoryAttrs -> m ()
discardCthulhuCard attrs = do
  push $ ScenarioSpecific "discardCthulhuCard" (toJSON $ toCard attrs)
  push $ StoryMessage $ RemoveStory attrs.id

{- | Where The Final Seal may place a sigil. Both Rooftop locations print "sigils
cannot be placed on it", and they are the only locations that do, so the restriction
lives here rather than as a modifier on each of them.
-}
canPlaceSigil :: LocationMatcher
canPlaceSigil = not_ (LocationWithTrait Rooftop)

{- | "While you are at [the Rooftops], you may fight and evade enemies at connecting
locations as if you were at their location."

Both Rooftop locations print this, so it lives here. The relaxation is attached to
the enemy's own fight and evade abilities, per investigator, via
@SetAbilityCriteria@ on @AbilityTarget iid ability.ref@ — the one hook
'getCanPerformAbility' consults. It cannot go on the enemy as an
@EnemyFightActionCriteria@ override, because for a basic fight or evade the
ability's source /is/ the enemy, and that path short-circuits before any such
override is read.

Scoping falls out of which (investigator, ability) pairs get the modifier at all:
only investigators standing on the rooftops, and only enemies at a connecting
location. That means the replacement criteria need not re-check location or
engagement — being on the rooftops is what "as if you were at their location"
grants, and Cthulhu's facets are Massive, so they are only ever engaged with
whoever is standing where they are.
-}
rooftopsReachConnecting
  :: ( HasGame m
     , Tracing m
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => LocationAttrs -> m ()
rooftopsReachConnecting a = do
  investigators <- select $ investigator_ (at_ (be a))
  unless (null investigators) do
    connecting <- select $ connectedFrom (be a)
    enemies <- select $ EnemyAt (mapOneOf LocationWithId connecting)
    for_ enemies \eid -> do
      fightAbilities <- select $ AbilityIs (EnemySource eid) AbilityAttack
      evadeAbilities <- select $ AbilityIs (EnemySource eid) AbilityEvade
      for_ investigators \iid -> do
        for_ fightAbilities \ability ->
          modified_ a (AbilityTarget iid ability.ref) [SetAbilityCriteria (CriteriaOverride fightCriteria)]
        for_ evadeAbilities \ability ->
          modified_ a (AbilityTarget iid ability.ref) [SetAbilityCriteria (CriteriaOverride evadeCriteria)]
 where
  -- Mirrors canFightCriteria minus its OnSameLocation check.
  fightCriteria =
    EnemyCriteria
      (ThisEnemy $ CanBeAttackedBy You <> EnemyOneOf [not_ AloofEnemy, EnemyIsEngagedWith Anyone])
      <> CanAttack
  -- Mirrors EvadeCriteria minus OnSameLocation and its "engaged with you" check.
  evadeCriteria =
    EnemyCriteria
      (ThisEnemy $ EnemyMatchAll [EnemyWithEvade, EnemyWithoutModifier CannotBeEvaded])

{- | "Draw the top card of the Cthulhu deck."

The Cthulhu deck is a scenario deck, so the draw goes through the normal machinery,
which already handles the deck running dry: drawing from an empty scenario deck
shuffles its discard pile back in and redraws, which is exactly the rule. The
scenario picks the drawn card up from @DrewCards@, resolves it, and discards it.
-}
drawCthulhuDeckCard :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
drawCthulhuDeckCard iid source = push $ DrawCards iid $ newCardDraw source CthulhuDeck 1

{- | "Return each [[Cthulhu]] enemy in the victory display to its place on the
Cthulhu Board, non-[[Enraged]] side faceup." Both Fight Back! and Banish Him! do
this when they advance, so it lives here.

Obtaining the card lifts it out of the victory display; it is then respawned from
its non-Enraged definition at Cthulhu's location, which is where the board's facets
stand.
-}
returnCthulhuFacetsToBoard :: ReverseQueue m => m ()
returnCthulhuFacetsToBoard = do
  victoryDisplay <- getVictoryDisplay
  selectOne (LocationWithEnemy (enemyIs Enemies.cthulhuAncientEvil)) >>= traverse_ \lid ->
    for_ cthulhuFacets \(front, enraged) ->
      for_ (find (`cardMatch` mapOneOf cardIs [front, enraged]) victoryDisplay) \card -> do
        obtainCard card
        createEnemyAt_ front lid

{- | "During an attack at your location" (Fight Back! / Banish Him!).

From either Rooftop location you "may fight and evade enemies at connecting
locations as if you were at their location", so an attack you are making on a
connecting location is happening at your location for this purpose too.
-}
attackAtYourLocation :: Criterion
attackAtYourLocation =
  oneOf
    [ DuringSkillTest $ WhileAttackingAnEnemy $ EnemyAt YourLocation
    , youExist (at_ (LocationWithTrait Rooftop))
        <> DuringSkillTest (WhileAttackingAnEnemy $ EnemyAt (connectedFrom YourLocation))
    ]
