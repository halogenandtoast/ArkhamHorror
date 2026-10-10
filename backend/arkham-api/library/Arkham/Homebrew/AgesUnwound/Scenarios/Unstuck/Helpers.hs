{- | Scenario IV's signature mechanics, in one place.

Three things in this scenario are printed on a dozen cards each, so none of them
re-derives it:

1. __The ring.__ The eight 'Adrift' locations in play sit in a circle built at
setup (Carnevale of Horrors is the precedent): each occupies one of
'ringLabels'' grid cells, and consecutive cells are chained with
'PlacedLocationDirection' so that __clockwise is @RightOf@__. Every agenda
prints "each [[Adrift]] location is connected to the locations clockwise and
counter-clockwise from it", which is the @connectsTo@ set each Adrift location
builder carries ('ringConnections'); the agenda text is realised there rather
than as a granted modifier, since an agenda is always in play and all four print
the same clause.

2. __"Choose a random [[Adrift]] location."__ The guide spells out the tabletop
procedure:

> This should be done by shuffling together the 8 locations removed from the
> game during setup (the versions of each [[Adrift]] location in play not
> currently being used) and drawing 1 at random. If you are instructed to choose
> a random location that fits a certain criterion, keep drawing locations until
> one is drawn that satisfies the effect's requirements.

The removed pile is a __randomiser__, not a destination: it holds exactly one
card per name in play, so drawing from it picks a uniformly random /in-play/
Adrift location, and "keep drawing until it qualifies" is a uniform pick over
the qualifying subset. That is 'getRandomAdriftLocation'.

3. __The end-of-turn 'Forced'.__ Every Adrift location prints one, and three
cards reach for it by name -- /The Endless Fall/ triggers it, /Heart of an
Empire/ suppresses it. It is therefore ability 'endOfTurnAbility' on every one
of them, with no exceptions, so 'triggerEndOfTurnForced' and
'ignoreEndOfTurnForcedAt' can name it.

Scenario-local on purpose: nothing outside Unstuck has a ring of Adrift
locations, so none of this belongs in the campaign's shared @Helpers.hs@.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers where

import Arkham.Card
import Arkham.Card.EncounterCard (lookupEncounterCard)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push, pushAll)
import Arkham.Classes.Query (select, selectOne)
import Arkham.Cost (Payment (NoPayment))
import Arkham.Direction
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (CannotTriggerAbilityMatching))
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Helpers (scenarioI18n)
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Label (mkLabel)
import Arkham.Matcher
import Arkham.Message (Message (..))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.Source
import GHC.Records

-- | Every Unstuck card and the scenario itself share this i18n scope.
unstuckI18n :: (HasI18n => a) -> a
unstuckI18n = scenarioI18n "unstuck"

-- * The ring

-- | How many locations the circle holds. Eight: one version of each pair.
ringSize :: Int
ringSize = 8

{- | The circle's grid cells, clockwise from the top. These are labels, not
locations: the scenario's layout template fixes where each cell is drawn, and a
location's /position/ in the ring is which label it currently wears.
-}
ringLabels :: [Text]
ringLabels = ["location" <> tshow n | n <- [1 .. ringSize]]

{- | "Each [[Adrift]] location is connected to the locations clockwise and
counter-clockwise from it." The Adrift locations print a Diamond badge and no
connection symbols at all, so this is the whole of their connectivity.
-}
ringConnections :: Set Direction
ringConnections = setFromList [LeftOf, RightOf]

{- | The two printings of each location that has them. Setup keeps one of each
pair and sets the other aside; agenda 3b swaps a pair over.
-}
adriftPairs :: [(CardDef, CardDef)]
adriftPairs =
  [ (Locations.aDisquietingFuture_069, Locations.aDisquietingFuture_070)
  , (Locations.aWorldAtWar_071, Locations.aWorldAtWar_072)
  , (Locations.anEarthLongDead_073, Locations.anEarthLongDead_074)
  , (Locations.arkhamMassachusetts_075, Locations.arkhamMassachusetts_076)
  , (Locations.banksOfTheNile_077, Locations.banksOfTheNile_078)
  , (Locations.heartOfAnEmpire_079, Locations.heartOfAnEmpire_080)
  , (Locations.millionsOfYearsAgo_081, Locations.millionsOfYearsAgo_082)
  , (Locations.theTatterdemalion_083, Locations.theTatterdemalion_084)
  ]

-- | 'adriftPairs' as the choices setup samples from.
adriftVersions :: [NonEmpty CardDef]
adriftVersions = [a :| [b] | (a, b) <- adriftPairs]

{- | "find the version of that location that was removed from the game" --
the other printing of the same place.
-}
otherPrinting :: CardDef -> Maybe CardDef
otherPrinting def =
  listToMaybe [other | (a, b) <- adriftPairs, (this, other) <- [(a, b), (b, a)], this == def]

{- | The ring, clockwise, read off the grid cells. Only locations currently
wearing a ring label are in it, so The Timestream and Arkham, Massachusetts are
excluded even once they are in play.
-}
getRing :: HasGame m => m [LocationId]
getRing = catMaybes <$> traverse (selectOne . LocationWithLabel . mkLabel) ringLabels

-- | @n@ steps clockwise around the ring, following the chained @RightOf@ edges.
getClockwise :: HasGame m => Int -> LocationId -> m (Maybe LocationId)
getClockwise n lid
  | n <= 0 = pure (Just lid)
  | otherwise = selectOne (rightOf lid) >>= maybe (pure Nothing) (getClockwise (n - 1))

-- | "the location across from you" -- half way round an eight-location circle.
getAcross :: HasGame m => LocationId -> m (Maybe LocationId)
getAcross = getClockwise (ringSize `div` 2)

{- | "swap the positions of your location and the location across from you".

Positions are the ring labels plus the directional edges between them, so the
whole circle is torn down and rebuilt around the new order. 'LocationMoved'
clears a location's own edges /and/ every reference to it from its neighbours,
which is what keeps the rebuild from appending to stale ones
('PlacedLocationDirection' inserts with @(<>)@). Everything else about the two
locations -- clues, enemies, attachments, the investigators standing on them --
is untouched: only where they sit in the circle changes.
-}
swapRingPositions :: (HasGame m, HasQueue Message m) => LocationId -> LocationId -> m ()
swapRingPositions lid1 lid2 = do
  ring <- getRing
  when (lid1 /= lid2 && lid1 `elem` ring && lid2 `elem` ring) do
    let
      exchange l
        | l == lid1 = lid2
        | l == lid2 = lid1
        | otherwise = l
      ring' = map exchange ring
    pushAll $ map LocationMoved ring'
    pushAll [SetLocationLabel l lbl | (l, lbl) <- zip ring' ringLabels]
    -- `PlacedLocationDirection r RightOf l` reads "r is to the right of l", so
    -- chaining consecutive cells (and closing the circle) makes clockwise
    -- travel RightOf and counter-clockwise LeftOf.
    pushAll
      [ PlacedLocationDirection r RightOf l
      | (l, r) <- zip ring' (drop 1 ring' <> take 1 ring')
      ]

-- * Choosing a random Adrift location

{- | "Choose a random [[Adrift]] location" fitting a criterion -- 'Anywhere' when
the effect names none. 'Nothing' only when nothing qualifies, which the guide's
"keep drawing" procedure cannot resolve either.
-}
getRandomAdriftLocation :: (HasGame m, MonadRandom m) => LocationMatcher -> m (Maybe LocationId)
getRandomAdriftLocation matcher = do
  lids <- select $ LocationWithTrait Adrift <> matcher
  traverse sample (nonEmpty lids)

{- | "move to a random other [[Adrift]] location" -- the agendas, the act's
failed move, /Heart of an Empire/ and /The Endless Fall/ all print it. "Other"
is read against the mover's current location.
-}
moveToRandomOtherAdrift
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
moveToRandomOtherAdrift source iid = do
  mlid <- getRandomAdriftLocation $ not_ (LocationWithInvestigator $ InvestigatorWithId iid)
  for_ mlid $ moveTo source iid

-- * The end-of-turn Forced

{- | The ability index every Adrift location's end-of-turn __Forced__ occupies.
Reserved on all sixteen printings, including the two Tatterdemalions whose
printed [reaction] therefore takes index 2.
-}
endOfTurnAbility :: Int
endOfTurnAbility = 1

{- | /The Endless Fall/: "trigger the forced ability on your location as if it
were the end of your turn". Resolving the ability directly rather than opening a
'TurnEnds' window keeps it to /that/ location, as printed -- a window would also
fire the treacheries in the investigator's threat area.
-}
triggerEndOfTurnForced :: ReverseQueue m => InvestigatorId -> LocationId -> m ()
triggerEndOfTurnForced iid lid =
  push $ UseCardAbility iid (toSource lid) endOfTurnAbility [] NoPayment

{- | /Heart of an Empire/: "Do not resolve any forced abilities on that location
that would trigger at the end of your turn." Scoped to the one investigator the
move displaced and to the rest of their turn.
-}
ignoreEndOfTurnForcedAt
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> LocationId -> m ()
ignoreEndOfTurnForcedAt source iid lid =
  turnModifier iid source iid
    $ CannotTriggerAbilityMatching (AbilityIsForcedAbility <> AbilityOnLocation (LocationWithId lid))

-- * The agenda backs

{- | "Discard each non-weakness treachery card in play. Each investigator at an
[[Adrift]] location moves to a random other [[Adrift]] location."

All four agenda backs print this (agenda 4b replaces it with "each remaining
investigator is defeated"), so none of them re-derives it. The discard runs
first, which is what keeps a treachery in a threat area from following its owner
across the ring.
-}
tumbleOutOfTime :: (ReverseQueue m, Sourceable source) => source -> m ()
tumbleOutOfTime source = do
  selectEach TreacheryIsNonWeakness $ toDiscard source
  investigators <- select $ InvestigatorAt (LocationWithTrait Adrift)
  for_ investigators $ moveToRandomOtherAdrift source

-- * Roman Soldiers

{- | /Determined General/ and /Roman Outpost/ both print "Put the top card of
your deck into play in your threat area, as a Roman Soldier enemy with 3 fight,
1 health, 3 evade, 1 damage and the [[Humanoid]] trait."

The stats come from the @:ages-unwound:900@ def (quantity 0, so it is never
gathered), and the copy is a real enemy built from that def carrying the /player
card's/ 'CardId' and owner -- the trick
"Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers" proved for
this campaign. It deliberately does /not/ rewrite the player card's
@pcCardCode@: that resolves only against @allPlayerCards <>
allSpecialEnemyCards@, which cannot see a homebrew def, and would fail silently.

Neither card prints a return path -- Unstuck has no "Copies of Enemies" rule --
so the ordinary one governs: a player card put into play goes to its __owner's
discard pile__ when it leaves play. That is the scenario's job, and it is why the
@(enemy, owner, card)@ triple has to be remembered: once the def is swapped in,
nothing on the enemy knows which player card it was made from.
-}
data RomanSoldier = RomanSoldier
  { romanSoldierEnemy :: EnemyId
  , romanSoldierOwner :: InvestigatorId
  , romanSoldierCard :: Card
  , romanSoldierReturned :: Bool
  {- ^ Set once the card has reached its owner's discard pile, so a second
  @RemoveEnemy@ for the same id cannot file it twice. The entry itself is
  kept, because the @Discarded@ suppression still has to recognise the id
  after the enemy is gone.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "enemy" RomanSoldier EnemyId where
  getField = romanSoldierEnemy

instance HasField "owner" RomanSoldier InvestigatorId where
  getField = romanSoldierOwner

instance HasField "card" RomanSoldier Card where
  getField = romanSoldierCard

instance HasField "returned" RomanSoldier Bool where
  getField = romanSoldierReturned

newtype UnstuckMeta = UnstuckMeta {unstuckRomanSoldiers :: [RomanSoldier]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "romanSoldiers" UnstuckMeta [RomanSoldier] where
  getField = unstuckRomanSoldiers

emptyUnstuckMeta :: UnstuckMeta
emptyUnstuckMeta = UnstuckMeta []

getUnstuckMeta :: HasGame m => m UnstuckMeta
getUnstuckMeta = toResultDefault emptyUnstuckMeta <$> scenarioField ScenarioMeta

{- | The message key the scenario answers to add one Roman Soldier to its meta.
A 'ScenarioSpecific' message rather than a @SetScenarioMeta@ read-modify-push,
because the latter reads the meta as it was when the handler ran: two soldiers
spawned in one handler would each write a one-element list and the second would
clobber the first.
-}
registerRomanSoldierKey :: Text
registerRomanSoldierKey = "unstuck.registerRomanSoldier"

-- | ... and the one that marks a soldier's card as already sent home.
returnedRomanSoldierKey :: Text
returnedRomanSoldierKey = "unstuck.returnedRomanSoldier"

{- | "Put the top card of your deck into play in your threat area, as a Roman
Soldier enemy ..."

An empty deck means no soldier: a card you do not have is a card you cannot
place.
-}
spawnRomanSoldier :: ReverseQueue m => InvestigatorId -> m ()
spawnRomanSoldier iid = do
  top <- fieldMap InvestigatorDeck (take 1 . (.cards)) iid
  for_ top \pc -> do
    let
      soldier =
        EncounterCard $ (lookupEncounterCard Enemies.romanSoldier pc.id) {ecOwner = Just iid}
    {- Passing the investigator spawns the soldier engaged with them, which is
    the 'InThreatArea' placement the card asks for. The /unlifted/ builder is
    used on purpose: it hands back the id *and* the message, so the triple can
    be registered before the enemy actually enters play. -}
    (eid, create) <- Msg.createEnemyWith soldier iid id
    scenarioSpecific registerRomanSoldierKey (RomanSoldier eid iid (PlayerCard pc) False)
    push $ ObtainCard pc.id
    push create
