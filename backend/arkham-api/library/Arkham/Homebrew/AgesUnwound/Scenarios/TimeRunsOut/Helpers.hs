{- | Scenario VII's signature mechanics, in one place.

Four things in the finale are printed on a dozen cards each, so none of them
re-derives it:

1. __The eight paired printings.__ Setup "puts one of the two versions of each of
the following locations into play at random, revealed side faceup ... Remove the
other versions of each of those locations from the game." Those are
'locationPairs'; the loser of each pair goes to the set-aside pool rather than
being dropped, exactly as Scenario IV's ring does, because later effects
(/Shattering Paradox/, agenda 1b) still have to find a named place and because
'otherPrinting' is how a card names its twin.

2. __"Choose a random location."__ The guide spells out the tabletop procedure:

> This should be done by shuffling together the 8 locations removed from the game
> during setup (the versions of each location in play not currently being used)
> and drawing 1 at random. If you are instructed to choose a random location that
> fits a certain criterion, keep drawing locations until one is drawn that
> satisfies the effect's requirements.

The removed pile is a __randomiser__, not a destination: it holds exactly one
card per name in play, so drawing from it picks a uniformly random /in-play/
location, and "keep drawing until it qualifies" is a uniform pick over the
qualifying subset. That is 'getRandomLocation'. (The three set-aside story
locations join the board at act 2b and have no twin in the pile; they are
ordinary locations from then on, and the guide's procedure would never pick them.
'getRandomLocation' deliberately reads the in-play set instead, so they are
eligible -- the pile is a physical stand-in for "a random location in play", and
once there are eleven places in play the stand-in no longer covers them.)

3. __"Warded".__ Act 1, act 2 and agenda 2 all print "Locations with a resource
on them are 'warded'." It is not a trait or a modifier, just a reading of the
resource count, so it is one matcher pair ('warded' / 'unwarded') that every card
shares.

4. __"Beneath the agenda deck".__ Agenda 1b and /Shattering Paradox/ put a
location there; act 4 pulls one back out, flips it and resolves its text. The
engine already has that zone as a first-class scenario field
('ScenarioCardsUnderAgendaDeck', fed by @PlaceUnderneath AgendaDeckTarget@), so
'placeBeneathAgendaDeck' is the in-play location turning back into a card and
'resolveFlippedLocation' is the dispatch on what its reverse face happens to be
-- in this set a story, an enemy, a treachery or a @[[Paradox]]@ location.

Scenario-local on purpose: nothing outside Time Runs Out has paired printings of
every location or a ward made of resources.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers where

import Arkham.Card
import Arkham.Card.EncounterCard (lookupEncounterCard)
import Arkham.ChaosToken.Types (ChaosTokenFace (ElderThing))
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select)
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Helpers.ChaosBag (getOnlyChaosTokensInBag)
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Location.Types (LocationAttrs)
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Message (pattern InvestigatorDrewEncounterCard, pattern ObtainCard)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Message.Lifted.Story (resolveStory)
import Arkham.Placement (Placement (InThreatArea))
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioCardsUnderAgendaDeck, ScenarioMeta))
import Arkham.Source
import Arkham.Target
import Arkham.Token qualified as Token
import GHC.Records

-- | Every Time Runs Out card and the scenario itself share this i18n scope.
timeRunsOutI18n :: (HasI18n => a) -> a
timeRunsOutI18n = scenarioI18n "timeRunsOut"

-- * The paired printings

{- | "Put one of the two versions of the following locations into play at random,
revealed side faceup ... Remove the other versions of each of those locations
from the game."

Printed order, which is also the order setup walks them in.
-}
locationPairs :: [(CardDef, CardDef)]
locationPairs =
  [ (Locations.fulcrumOfPossibility_189, Locations.fulcrumOfPossibility_190)
  , (Locations.daysThatNeverWere_191, Locations.daysThatNeverWere_192)
  , (Locations.secretsLongForgotten_193, Locations.secretsLongForgotten_194)
  , (Locations.todayAThousandTimes_195, Locations.todayAThousandTimes_196)
  , (Locations.whatCouldBe_197, Locations.whatCouldBe_198)
  , (Locations.whatCouldNeverBe_199, Locations.whatCouldNeverBe_200)
  , (Locations.dawnOfTheUniverse_201, Locations.dawnOfTheUniverse_202)
  , (Locations.theEndOfAllThings_203, Locations.theEndOfAllThings_204)
  ]

-- | 'locationPairs' as the choices setup samples from.
locationVersions :: [NonEmpty CardDef]
locationVersions = [a :| [b] | (a, b) <- locationPairs]

-- | The other printing of the same place -- "the version removed from the game".
otherPrinting :: CardDef -> Maybe CardDef
otherPrinting def =
  listToMaybe [other | (a, b) <- locationPairs, (this, other) <- [(a, b), (b, a)], this == def]

-- * Warding

{- | "Locations with a resource on them are 'warded'." A reading of the resource
count, not a trait: a card that removes the resource unwards the place.
-}
warded :: LocationMatcher
warded = LocationWithResources (atLeast 1)

-- | The complement of 'warded'. Agenda 1b and /Fateweaver/ both hunt for these.
unwarded :: LocationMatcher
unwarded = LocationWithResources (atMost 0)

-- | "Place 1 resource on your location" -- the act 2 ability's ward.
wardLocation :: (ReverseQueue m, Sourceable source) => source -> LocationId -> m ()
wardLocation source lid = placeTokens source lid Token.Resource 1

-- * Choosing a random location

{- | "Choose a random location" fitting a criterion -- 'Anywhere' when the effect
names none. 'Nothing' only when nothing qualifies, which the guide's "keep
drawing" procedure cannot resolve either.
-}
getRandomLocation :: (HasGame m, MonadRandom m) => LocationMatcher -> m (Maybe LocationId)
getRandomLocation matcher = do
  lids <- select matcher
  traverse sample (nonEmpty lids)

{- | "If you succeed, either move to any other location, or move another
investigator to this location."

Both printings of /Fulcrum of Possibility/ print it, so neither re-derives it.
The destinations go through 'getCanMoveToMatchingLocations' so a movement ban is
respected, and each branch is offered only when it has a target -- with eight
places on the table the first always does, but the second does not in a solo
game.
-}
fulcrumMove
  :: (ReverseQueue m, Sourceable source)
  => source
  -> LocationAttrs
  -> InvestigatorId
  -> m ()
fulcrumMove source attrs iid = do
  destinations <- getCanMoveToMatchingLocations iid source (not_ $ be attrs)
  others <- select $ not_ (InvestigatorWithId iid)
  when (notNull destinations || notNull others)
    $ chooseOrRunOneM iid
    $ timeRunsOutI18n
    $ scope "fulcrumOfPossibility" do
      labeledValidate (notNull destinations) "moveToAnotherLocation"
        $ chooseTargetM iid destinations (moveTo source iid)
      labeledValidate (notNull others) "moveAnotherInvestigatorHere"
        $ chooseTargetM iid others \other -> moveTo source other attrs

-- * Beneath the agenda deck

{- | "place that location underneath the agenda deck"

The location leaves play and becomes a card again. 'removeLocation' would divert
a Victory X location to the victory display, and a location shelved under the
agenda deck has not been overcome, so this takes the no-victory path -- as
Scenario III's act does when it removes a location from the game.
-}
placeBeneathAgendaDeck :: ReverseQueue m => LocationId -> m ()
placeBeneathAgendaDeck lid = do
  card <- field Field.LocationCard lid
  removeLocationWithoutVictory lid
  placeUnderneath AgendaDeckTarget [card]

-- | The locations act 4 is waiting to clear.
getLocationsBeneathAgendaDeck :: HasGame m => m [Card]
getLocationsBeneathAgendaDeck = scenarioField ScenarioCardsUnderAgendaDeck

{- | "randomly choose one of the locations beneath the agenda deck, flip it, and
resolve its text" -- act 4's __Forced__.

Flipping is 'forceFlipCard', not 'flipCard': these cards are printed
@otherSideIs@, which sets @cdDoubleSided = False@, and 'flipCard' answers that by
merely clearing the card's flipped flag instead of swapping in the other side.
Only 'forceFlipCard' reads @cdOtherSide@ unconditionally.

What the other face /is/ differs per location -- in this set a story, an enemy, a
treachery or a @[[Paradox]]@ location -- so the resolution dispatches on its card
type. Every one of them ends by removing itself from the game or by entering
play, so the card is obtained out from under the agenda deck first.

An enemy or treachery face is resolved by handing the flipped card to the lead as
a drawn encounter card, because that is the only path that runs a __Revelation__
and a __Spawn__ instruction. It therefore also opens a card-draw window the
printed wording does not -- nothing in this scenario reacts to an encounter draw,
so it is inert here, but it is the one place where "flip it" and "draw it" are not
the same thing.
-}
resolveFlippedLocation :: ReverseQueue m => Card -> m ()
resolveFlippedLocation card = do
  obtainCard card
  let flipped = forceFlipCard card
  lead <- getLead
  case toCardType flipped of
    StoryType -> resolveStory lead flipped
    LocationType -> placeLocation_ flipped
    _ -> case flipped of
      EncounterCard ec -> push $ InvestigatorDrewEncounterCard lead ec
      _ -> pure ()

-- * Exile

{- | "exile a non-weakness asset you control"

The campaign's Exile rules add the rest: "If a game effect forces you to choose a
target to exile, weaknesses and permanent assets are not valid targets." A
permanent asset cannot leave play at all, so it is excluded here rather than on
each card.
-}
exileableAsset :: AssetMatcher
exileableAsset = NonWeaknessAsset <> not_ PermanentAsset <> AssetControlledBy You

{- | 'exileableAsset' for a named investigator, which is what a handler has once
the ability has been triggered.
-}
exileableAssetOf :: InvestigatorId -> AssetMatcher
exileableAssetOf iid =
  NonWeaknessAsset <> not_ PermanentAsset <> AssetControlledBy (InvestigatorWithId iid)

-- | "exile a non-weakness asset you control", as a prompt.
chooseAndExileAsset :: ReverseQueue m => InvestigatorId -> m ()
chooseAndExileAsset iid = do
  assets <- select (exileableAssetOf iid)
  chooseTargetM iid assets exile

{- | "exile a card from your hand"

Used by the scenario's own [elder_thing] token and by /The End of All Things/'s
__Oblivion Beckons__. The Exile rules exclude weaknesses from a forced choice.
-}
chooseAndExileFromHand :: ReverseQueue m => InvestigatorId -> m ()
chooseAndExileFromHand iid = do
  cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
  chooseOneM iid $ for_ cards \card -> targeting card (exile card)

{- | "a card ... of level 1 or higher"

Spelled out rather than negating 'CardWithMaxLevel': a card with no printed level
at all would pass @not_ (CardWithMaxLevel 0)@, and a story asset sitting in hand
is not "level 1 or higher".
-}
levelOneOrHigher :: CardMatcher
levelOneOrHigher = mapOneOf CardWithLevel [1 .. 5]

-- * Searching the collection

{- | "Search your collection for ... Add that card to your hand."

The engine has no collection search with a /choice/: 'SearchCollectionForRandom'
is built on the deck-building weakness sampler and can only ever return a basic
weakness. The one official precedent for reaching into the collection at all is
/Memories of Another Life (5)/, which offers every matching 'CardDef' in
@allPlayerCards@ as a card choice, and this is that shape. Each candidate is
generated only to carry an image into the prompt and is deregistered again
straight away; the choice travels as a 'CardCodeTarget', and the caller's
@HandleTargetChoice@ generates the real copy.

TODO(ages-unwound): a shared @searchCollection@ seam with a chooser would be the
right home for this; it is deliberately not invented here.
-}
chooseCollectionCard
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> (CardDef -> Bool) -> m ()
chooseCollectionCard iid source p = do
  let defs = filter p (toList allPlayerCards)
  unless (null defs) $ chooseOneM iid $ for_ defs \def -> do
    card <- genCard def
    removeCard card.id
    cardLabeled card $ handleTarget iid source (CardCodeTarget card.cardCode)

{- | "a non-exceptional player card of level at most X" -- /Days That Never Were/.

Weaknesses are excluded: a weakness is a player card, but adding one to your hand
is not a search result anybody would pick, and the campaign's own Exile rules
already treat weaknesses as outside the set of things these effects may name.
-}
nonExceptionalPlayerCardAtMost :: Int -> CardDef -> Bool
nonExceptionalPlayerCardAtMost n def =
  def.kind
    `elem` [AssetType, EventType, SkillType]
    && maybe False (<= n) def.level
    && not def.exceptional
    && isNothing (cdCardSubType def)

-- | "a level 0 skill card" -- /Secrets Long Forgotten/.
levelZeroSkill :: CardDef -> Bool
levelZeroSkill def =
  def.kind == SkillType && def.level == Just 0 && isNothing (cdCardSubType def)

-- * The Elder Thing supply

{- | How many @[elder_thing]@ tokens the physical game has, and therefore how many
Resolution 3 can ever add.

Resolution 3 reads "Add 1 [elder_thing] token to the chaos bag. __If you cannot__,
each investigator takes 1 mental trauma instead", and that clause is the only
thing bounding how many times the finale can be replayed. The engine has no token
supply limit of its own for this face -- 'Arkham.Helpers.ChaosBag.canAddChaosTokenFace'
answers @True@ forever, because @chaosTokenFacePool ElderThing@ is @Nothing@ --
so read literally the condition would never fire and the back-edge would loop
without end. The cap is therefore modelled here, at the box's four tokens, rather
than ignored: it is the designer's stated bound, it is local to this resolution,
and putting @ElderThing@ into the engine's face pool would change every scenario
that adds one.
-}
elderThingSupply :: Int
elderThingSupply = 4

{- | "Add 1 [elder_thing] token to the chaos bag. If you cannot ..." -- whether the
supply has a token left. See 'elderThingSupply'.
-}
canAddElderThing :: HasGame m => m Bool
canAddElderThing = do
  inBag <- count ((== ElderThing) . (.face)) <$> getOnlyChaosTokensInBag
  pure $ inBag < elderThingSupply

-- * Yourself

{- | Act 2b: "Put the set-aside Yourself enemy next to the agenda deck. Each
investigator puts the top card of their deck facedown in their threat area, as a
copy of Yourself. 'You' on each copy of Yourself refers to the owner of the card."

Every clause on the card is read against that owner, which for a copy in a threat
area is simply the investigator whose threat area it sits in.
-}
yourselfOwner :: HasGame m => EnemyId -> m (Maybe InvestigatorId)
yourselfOwner eid =
  field EnemyPlacement eid <&> \case
    InThreatArea iid -> Just iid
    _ -> Nothing

{- | A copy of /Yourself/ and the player card it was made from.

The copy is a real enemy built from the @:ages-unwound:208@ def carrying the
/player card's/ 'CardId' and owner -- the trick
"Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers" proved for this
campaign and Scenario IV reused for its Roman Soldiers. Once the def is swapped in
nothing on the enemy knows which player card it stood in for, so the triple has to
be remembered by the scenario.
-}
data YourselfCopy = YourselfCopy
  { yourselfCopyEnemy :: EnemyId
  , yourselfCopyOwner :: InvestigatorId
  , yourselfCopyCard :: Card
  , yourselfCopyReturned :: Bool
  {- ^ Set once the card has reached its owner's discard pile, so a second
  @RemoveEnemy@ for the same id cannot file it twice.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "enemy" YourselfCopy EnemyId where
  getField = yourselfCopyEnemy

instance HasField "owner" YourselfCopy InvestigatorId where
  getField = yourselfCopyOwner

instance HasField "card" YourselfCopy Card where
  getField = yourselfCopyCard

instance HasField "returned" YourselfCopy Bool where
  getField = yourselfCopyReturned

newtype TimeRunsOutMeta = TimeRunsOutMeta {timeRunsOutYourselves :: [YourselfCopy]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "yourselves" TimeRunsOutMeta [YourselfCopy] where
  getField = timeRunsOutYourselves

emptyTimeRunsOutMeta :: TimeRunsOutMeta
emptyTimeRunsOutMeta = TimeRunsOutMeta []

getTimeRunsOutMeta :: HasGame m => m TimeRunsOutMeta
getTimeRunsOutMeta = toResultDefault emptyTimeRunsOutMeta <$> scenarioField ScenarioMeta

{- | The message key the scenario answers to add one copy of /Yourself/ to its
meta. A 'Arkham.Message.ScenarioSpecific' message rather than a
@SetScenarioMeta@ read-modify-push, because the latter reads the meta as it was
when the handler ran: four copies spawned in one handler would each write a
one-element list and the last would clobber the rest.
-}
registerYourselfKey :: Text
registerYourselfKey = "timeRunsOut.registerYourself"

-- | ... and the one that marks a copy's card as already sent home.
returnedYourselfKey :: Text
returnedYourselfKey = "timeRunsOut.returnedYourself"

{- | "Each investigator puts the top card of their deck facedown in their threat
area, as a copy of Yourself."

An empty deck means no copy: a card you do not have is a card you cannot place.
-}
spawnYourself :: ReverseQueue m => InvestigatorId -> m ()
spawnYourself iid = do
  top <- fieldMap InvestigatorDeck (take 1 . (.cards)) iid
  for_ top \pc -> do
    let copy = EncounterCard $ (lookupEncounterCard Enemies.yourself pc.id) {ecOwner = Just iid}
    {- Passing the investigator spawns the copy engaged with them, which is the
    'InThreatArea' placement the card asks for -- and the placement
    'yourselfOwner' reads every "you" on the card off. The /unlifted/ builder is
    used on purpose: it hands back the id *and* the message, so the triple can be
    registered before the enemy actually enters play. -}
    (eid, create) <- Msg.createEnemyWith copy iid id
    scenarioSpecific registerYourselfKey (YourselfCopy eid iid (PlayerCard pc) False)
    push $ ObtainCard pc.id
    push create
