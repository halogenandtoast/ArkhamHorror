module Arkham.Homebrew.DarkMatter.Helpers where

import Arkham.Ability
import Arkham.Actions (Actions (..))
import Arkham.Agenda.Types (AgendaAttrs)
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Calculation (
  GameCalculation (
    CountAssets,
    CountEnemies,
    CountTreacheries,
    ScenarioInDiscardCountCalculation,
    SumCalculation
  ),
 )
import Arkham.CampaignLog (campaignLogRecordedCounts)
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosTokenFace (..))
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push, pushAll)
import Arkham.Classes.Query (select, selectAny, selectCount, selectOne, selectWithField)
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.Enemy.Types (Field (EnemyCardsUnderneath))
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv (getCurrentBatchId)
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Game (getRemovedFromPlayCards)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Location (replaceLocation, withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (
  getAgendaDeckCards,
  getEncounterDeck,
  getScenarioDeck,
  scenarioField,
 )
import Arkham.Helpers.Window (wouldWindows)
import Arkham.Helpers.Xp
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern EvidenceDeck, pattern ScanningDeck)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain, pattern Carcosa)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorLog, InvestigatorMentalTrauma))
import Arkham.Location.Types (Field (LocationCard, LocationPrintedSymbol), LocationAttrs)
import Arkham.LocationSymbol
import Arkham.Matcher (
  AssetMatcher (AssetAt, AssetFacedownInThreatAreaOf, AssetWithPlacement, AssetWithTrait),
  CardMatcher (AnyCard, CardWithTrait),
  EnemyMatcher (EnemyFacedownInThreatAreaOf, EnemyWithPlacement, IncludeOutOfPlayEnemy),
  InvestigatorMatcher (Anyone, InvestigatorAt, InvestigatorCanGainXp, InvestigatorWithId, You),
  LocationMatcher (
    LocationCanBeFlipped,
    LocationInDirection,
    LocationWithAsset,
    LocationWithCardId,
    LocationWithEnemy,
    LocationWithId,
    LocationWithToken,
    LocationWithTrait,
    NearestLocationTo
  ),
  TreacheryMatcher (..),
  WindowMatcher (CampaignEvent, ScenarioEvent),
  assetIs,
  atLeast,
  connectedTo,
  enemyIs,
  locationIs,
  locationWithAsset,
  locationWithInvestigator,
  mapOneOf,
  oneOf,
  pattern LocationWithoutEnemies,
  pattern LocationWithoutInvestigators,
 )
import Arkham.Message (
  Message (
    CampaignSpecific,
    DrewCards,
    Flip,
    IncrementRecordCountForInvestigator,
    PlaceTreachery,
    ReplaceLocation,
    ResolveTreachery,
    Revelation,
    SetCardAside,
    ShuffleCardsIntoDeck,
    StoryMessage,
    Would
  ),
  ReplaceStrategy (Swap),
  resolve,
  pattern InvestigatorDrawEnemy,
  pattern RemoveLocation,
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Message.Lifted.Placement qualified as Placement
import Arkham.Message.Lifted.Story (resolveStory)
import Arkham.Message.Story (StoryMessage (RemoveStory))
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Setup
import Arkham.Scenario.Types (Field (ScenarioDiscard, ScenarioSetAsideCards))
import Arkham.Source
import Arkham.Story.Types (StoryAttrs)
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Cave, Crew))
import Arkham.Window qualified as Window
import Arkham.Xp

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "darkMatter" a

scenarioI18n :: Scope -> (HasI18n => a) -> a
scenarioI18n scenarioScope a = campaignI18n $ scope scenarioScope a

-- ** Experience (Heir to Carcosa) ** --

{- | Award scenario-resolution experience, including Heir to Carcosa's reward.
The reward is handled here rather than as an XP modifier because ties for the
least mental trauma require a player choice.
-}
earnXp :: (HasI18n, ReverseQueue m, Sourceable source) => source -> Scope -> m ()
earnXp source resolutionKey = earnXpWithBonus source resolutionKey NoBonus

earnXpWithBonus
  :: (HasI18n, ReverseQueue m, Sourceable source) => source -> Scope -> XpBonus -> m ()
earnXpWithBonus source resolutionKey bonus = do
  heirIsInPlay <- selectAny $ assetIs Assets.heirToCarcosa
  traumas <- selectWithField InvestigatorMentalTrauma InvestigatorCanGainXp
  case (heirIsInPlay, sortOn snd traumas) of
    (True, (_, leastTrauma) : _) | leastTrauma > 0 -> do
      let candidates = map fst $ takeWhile ((== leastTrauma) . snd) $ sortOn snd traumas
      lead <- getLead
      chooseOrRunOneM lead $ targets candidates $ awardXp . Just . (,leastTrauma)
    _ -> awardXp Nothing
 where
  awardXp :: (HasI18n, ReverseQueue n) => Maybe (InvestigatorId, Int) -> n ()
  awardXp heirReward = resolutionWithXp resolutionKey do
    (initial, details) <- getXpWithBonus' bonus.value
    let
      addReward (iid, amount) = map $ \(iid', xp) -> (iid', xp + if iid == iid' then amount else 0)
      details' = maybe details (`addReward` details) heirReward
      rewardEntries = case heirReward of
        Nothing -> mempty
        Just (iid, amount) ->
          XpBreakdown
            [ InvestigatorGainXp iid
                $ XpDetail XpFromCardEffect "$darkMatter.xp.heirToCarcosa" amount
            ]
    Msg.push . Msg.ReportXp . (<> rewardEntries) =<< generateXpReport bonus
    Msg.pushAll =<< toGainXp source (pure details')
    pure initial

-- ** Memories (guide p3) ** --

-- "Memories" are tied to specific investigators and are not shared. They live
-- as a tally in each investigator's own campaign log section.

getMemories :: HasGame m => InvestigatorId -> m Int
getMemories =
  fieldMap InvestigatorLog (findWithDefault 0 (toCampaignLogKey Memories) . campaignLogRecordedCounts)

addMemories :: ReverseQueue m => InvestigatorId -> Int -> m ()
addMemories iid n = push $ IncrementRecordCountForInvestigator iid (toCampaignLogKey Memories) n

-- | Cross out tally marks. The number of Memories never drops below zero.
crossOffMemories :: ReverseQueue m => InvestigatorId -> Int -> m ()
crossOffMemories iid n = addMemories iid (negate n)

-- ** Impending Doom ** --

getImpendingDoom :: HasGame m => m Int
getImpendingDoom = getRecordCount ImpendingDoom

addImpendingDoom :: ReverseQueue m => Int -> m ()
addImpendingDoom = incrementRecordCount ImpendingDoom

-- ** Scan and the scanning deck (guide p2) ** --

-- Scanning-back cards declare the icons printed at the bottom of their back
-- via the @"scanIcons"@ card-def meta key, e.g.
-- @withMeta ("scanIcons", toJSON [Circle, Moon]) $ location_ ...@.
-- For locations, the icon referred to by scan abilities is the connection
-- symbol in the top left corner of the card (its 'cdLocationSymbol').

scanIcons :: HasCardDef a => a -> [LocationSymbol]
scanIcons a = fromMaybe [] $ lookup "scanIcons" (cdMeta $ toCardDef a) >>= maybeResult

hasScanningBack :: HasCardDef a => a -> Bool
hasScanningBack = notNull . scanIcons

{- | Icons printed on the *front* of a card — Strange Moons' [[Brain]] story
assets each show one. These are the query side of a scan (the icon you scan
*for*), where 'scanIcons' is the match side (the icons a scanning back shows).
-}
printedIcons :: HasCardDef a => a -> [LocationSymbol]
printedIcons a = fromMaybe [] $ lookup "printedIcons" (cdMeta $ toCardDef a) >>= maybeResult

{- | "a [[Brain]] story asset attached to this location". Strange Moons'
[[Interface]] locations scan for their own icon plus the icon of a brain
attached to them.
-}
brainAttachedTo :: LocationId -> AssetMatcher
brainAttachedTo lid = AssetWithTrait Brain <> AssetWithPlacement (AttachedToLocation lid)

brainsAttachedTo :: HasGame m => LocationId -> m [AssetId]
brainsAttachedTo = select . brainAttachedTo

{- | "the nearest [[Brain]] story asset" (Innocent Mishap, and the [skull] token
on Hard/Expert). Brains only ever sit attached to a location, so the nearest one
is any brain on the nearest location holding one. Ties are left for the player.
-}
nearestBrain :: InvestigatorId -> AssetMatcher
nearestBrain iid =
  AssetWithTrait Brain
    <> AssetAt (NearestLocationTo iid $ LocationWithAsset (AssetWithTrait Brain))

{- | Setup: "Create the scanning deck. This is done by taking all the (other)
encounter cards with icons at the bottom of their back side and shuffling
them together." Call after setting aside any scanning-back cards that the
setup excludes (they are gone from the gathered pool by then).
-}
addScanningDeck :: ReverseQueue m => ScenarioBuilderT m ()
addScanningDeck = do
  cards <- filter hasScanningBack <$> amongGathered AnyCard
  removeCards cards
  addExtraDeck ScanningDeck =<< shuffle cards

getScanningDeck :: HasGame m => m [Card]
getScanningDeck = getScenarioDeck ScanningDeck

{- | The Scan action designator; usually, but not always, initiated using the
"activate" action.
-}
scanAction :: Cost -> AbilityType
scanAction cost = ActionAbility (SingleAction Scan) Nothing (ActionCost 1 <> cost)

scanAction_ :: AbilityType
scanAction_ = scanAction mempty

{- | Payload shared by every 'Window.CampaignEvent' in the @scan@ family fired
after a scan, successful or not. Read it with 'getScanResult'.
-}
data ScanResult = ScanResult
  { scannedBy :: InvestigatorId
  , scannedFor :: [LocationSymbol]
  , scannedCard :: Maybe Card
  , scanSuccessful :: Bool
  , scannedAt :: Maybe LocationId
  {- ^ Where the scan was performed, captured *before* the scanned card is
  drawn. A scanned location is put into play and can move the scanning
  investigator to it, so by the time the @scan@ windows fire "your location"
  may already be the location the scan produced. 'Maybe' both because an
  investigator can be nowhere and so window payloads persisted before this
  field existed still parse.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Fired after every scan, successful or not. Scanning is campaign-wide, so
this is a 'Window.CampaignEvent' like 'wouldScanEvent'.
-}
scanEvent :: Text
scanEvent = "scan"

{- | Companions to 'scanEvent'. Every scan fires @scan@ plus one narrower key
per thing a card might care about, so a card that only reacts to *some* scans
matches its window directly instead of triggering on every scan and re-checking
the payload — the same bracketed-key convention the Scarlet Keys concealed cards
use (@noConcealed[<kind>]@).

Without these, a reaction is offered (and a forced ability queued) after every
scan in the campaign and then silently does nothing, which reads to the player
as a broken card.
-}
scanEventKey :: Text -> Text
scanEventKey key = scanEvent <> "[" <> key <> "]"

-- | A scan for [Trefoil] fires @scan[Trefoil]@.
scanEventFor :: LocationSymbol -> Text
scanEventFor = scanEventKey . tshow

{- | @scan[at:<location id>]@ — the location the scan was performed at. Cards
printing "After you scan at this location" must match this rather than pairing
the broad @scan@ key with a 'Here' criterion: a successful scan can put the
scanned location into play and move the investigator to it, so 'Here' is
evaluated against wherever the scan *landed* them.
-}
scanEventAt :: LocationId -> Text
scanEventAt lid = scanEventKey ("at:" <> tshow lid)

-- | @scan[successful]@ / @scan[unsuccessful]@: did the scan find a card?
successfulScanEvent :: Text
successfulScanEvent = scanEventKey "successful"

unsuccessfulScanEvent :: Text
unsuccessfulScanEvent = scanEventKey "unsuccessful"

-- | @scan[<card code>]@: the specific card the scan drew.
scanEventForCard :: HasCardCode a => a -> Text
scanEventForCard = scanEventKey . unCardCode . toCardCode

{- | @scan[AssetType]@: the type of the card the scan drew. The card back is not
part of what a scan cares about — "a story asset" is one thing to a player — so
'EncounterAssetType' folds into 'AssetType' and both sides of the window agree
on the single key.
-}
scanEventForCardType :: CardType -> Text
scanEventForCardType =
  scanEventKey . tshow . \case
    EncounterAssetType -> AssetType
    cardType -> cardType

{- | Announce a finished scan. All the keys fire as one window batch so that
reactions to the same scan are simultaneous, rather than the narrower keys
resolving after the general one.
-}
checkScanWindows :: ReverseQueue m => ScanResult -> m ()
checkScanWindows r = do
  let
    event key = Window.mkAfter $ Window.CampaignEvent key (Just $ scannedBy r) (toJSON r)
    ks =
      scanEvent
        : (if scanSuccessful r then successfulScanEvent else unsuccessfulScanEvent)
        : map scanEventFor (ordNub $ scannedFor r)
          <> map scanEventAt (toList $ scannedAt r)
          <> concat
            [ [scanEventForCard card, scanEventForCardType (toCardType card)]
            | card <- toList (scannedCard r)
            ]
  checkWindows $ map event ks

-- | Is this window key @scan@ or one of its narrower @scan[...]@ companions?
isScanEvent :: Text -> Bool
isScanEvent key = key == scanEvent || (scanEvent <> "[") `isPrefixOf` key

{- | Read the 'ScanResult' out of whichever scan window triggered an ability.
Every key in the family carries the same payload, so cards can match the
narrowest window that fits and still read the details here.
-}
getScanResult :: [Window.Window] -> Maybe ScanResult
getScanResult = \case
  [] -> Nothing
  (Window.windowType -> Window.CampaignEvent key _ v) : _ | isScanEvent key -> Just (toResult v)
  _ : rest -> getScanResult rest

{- | The @#when@ window before any scan resolves. Mount Sinai and Threshold of
Yuggoth print "When you would scan at <location>: ..." and cancel the scan on a
bad result; they do so by popping the pending 'doScanKey' message, exactly as
core's cancel effects pop the effect they are cancelling.

Scanning is campaign-wide, so both the window and the deferred scan are campaign
events rather than scenario events.
-}
wouldScanEvent :: Text
wouldScanEvent = "wouldScan"

-- | Payload of the deferred scan: who is scanning, from what, and for which icons.
doScanKey :: Text
doScanKey = "doScan"

data PendingScan = PendingScan
  { pendingScanBy :: InvestigatorId
  , pendingScanSource :: Source
  , pendingScanFor :: [LocationSymbol]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Perform a scan for the given icon(s). A card matches only if it shows every
requested icon (Strange Moons' "Brain Scanning" scans for two icons; a normal
scan passes one).

The scan is deferred behind a @#when@ window so that "when you would scan"
effects can resolve their own skill tests and cancel it. 'runPendingScan' below
does the work once the window has passed.
-}
scan
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> [LocationSymbol] -> m ()
scan iid (toSource -> source) icons = do
  (batchId, windowMessages) <-
    wouldWindows $ Window.CampaignEvent wouldScanEvent (Just iid) (toJSON icons)
  push
    $ Would batchId
    $ windowMessages
    <> [CampaignSpecific doScanKey (toJSON $ PendingScan iid source icons)]

{- | Cancel a scan that has been announced but not yet resolved, from inside its
@wouldScan@ window. The whole batch goes, not just the one message, so anything
else the scan queues dies with it.
-}
cancelPendingScan :: ReverseQueue m => m ()
cancelPendingScan = getCurrentBatchId >>= traverse_ cancelBatch

{- | Resolve a scan announced by 'scan'. Non-matching cards are set aside face
down and shuffled back in afterwards; the first matching card is drawn. If no
card matches, the scan is unsuccessful.
-}
runPendingScan :: ReverseQueue m => PendingScan -> m ()
runPendingScan (PendingScan iid source icons) = do
  deck <- getScanningDeck
  -- Before 'drawScannedCard', which can put a scanned location into play and
  -- move the investigator onto it.
  scannedAt' <- getMaybeLocation iid
  let matches c = all (`elem` scanIcons c) icons
  case break matches deck of
    (skipped, []) -> do
      unless (null skipped) $ setScenarioDeck ScanningDeck =<< shuffle skipped
      checkScanWindows $ ScanResult iid icons Nothing False scannedAt'
    (skipped, x : rest) -> do
      deck' <- if null skipped then pure rest else shuffle (skipped <> rest)
      setScenarioDeck ScanningDeck deck'
      drawScannedCard iid source x
      checkScanWindows $ ScanResult iid icons (Just x) True scannedAt'

-- | "Scan ... with an icon matching your current location" — the usual form.
scanAtYourLocation :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
scanAtYourLocation iid source = withLocationOf iid \lid -> do
  symbol <- field LocationPrintedSymbol lid
  scan iid source [symbol]

{- | "If it is a location, put it into play and move to it." Drawing the scanned
card normally places the location itself, so this only places it as a fallback.
-}
moveToScannedLocation
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> ScanResult -> m ()
moveToScannedLocation source iid r = for_ (scannedCard r) \card -> do
  lid <- selectOne (LocationWithCardId card.id) >>= maybe (placeLocation card) pure
  moveTo source iid lid

{- | Motion scanning (In the Shadow of Earth): simply draw the top card of the
scanning deck. The caller is responsible for the "only while at a location
with a matching icon" restriction.
-}
scanTopOfScanningDeck
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
scanTopOfScanningDeck iid (toSource -> source) = do
  deck <- getScanningDeck
  scannedAt' <- getMaybeLocation iid
  case deck of
    [] ->
      checkScanWindows $ ScanResult iid [] Nothing False scannedAt'
    (x : rest) -> do
      setScenarioDeck ScanningDeck rest
      drawScannedCard iid source x
      checkScanWindows $ ScanResult iid (scanIcons x) (Just x) True scannedAt'

{- | Draw a scanned card. A scanned *location* is put into play on top of Reality
Simulator instead — that location prints "(Reminder - Reality Simulator is not in
play while there is a card on top of it)", and Dream Diagnostics and Memory
Scanner are the only things that can scan one up.

Not a 'Swap': the [[Simulation]] locations arrive with their own clue value, and
only 'DefaultReplace' pushes the @PlacedLocation@ that places it. Reality
Simulator's card goes underneath so it can take its place again when the
location on top leaves play, per the scenario's "Replacing Locations" rules box.
-}
drawScannedCard :: ReverseQueue m => InvestigatorId -> Source -> Card -> m ()
drawScannedCard iid source card | toCardType card == LocationType = do
  selectOne (locationIs Locations.realitySimulator) >>= \case
    Just lid -> do
      simulator <- field LocationCard lid
      replaceLocation lid card
      placeUnderneath lid [simulator]
    Nothing -> drawScannedCard' iid source card
drawScannedCard iid source card = drawScannedCard' iid source card

drawScannedCard' :: ReverseQueue m => InvestigatorId -> Source -> Card -> m ()
drawScannedCard' iid source card = do
  case card.kind of
    StoryType -> handleCard
    _ -> do
      focusCards [card] $ chooseTargetM iid [card] (const unfocusCards)
      handleCard
 where
  handleCard =
    push
      $ DrewCards iid
      $ CardDrew
        { cardDrewSource = source
        , cardDrewDeck = Deck.ScenarioDeckByKey ScanningDeck
        , cardDrewCards = [card]
        , cardDrewAction = False
        , cardDrewRules = mempty
        , cardDrewTarget = Nothing
        }

{- | Several Strange Moons story cards end with "shuffle this card back into the
scanning deck": the story leaves play and its card rejoins the deck.
-}
returnToScanningDeck :: ReverseQueue m => StoryAttrs -> m ()
returnToScanningDeck attrs = do
  push $ StoryMessage $ RemoveStory attrs.id
  shuffleIntoScanningDeck [toCard attrs]

{- | "If such a situation arises that you would need to discard a card with
the scanning back or shuffle it into any other deck, shuffle it back into
the scanning deck instead."
-}
shuffleIntoScanningDeck :: (ReverseQueue m, IsCard card) => [card] -> m ()
shuffleIntoScanningDeck cards =
  push $ ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey ScanningDeck) (map toCard cards)

-- | "an empty location without a resource token on it"
emptyUnstabilizedLocation :: LocationMatcher
emptyUnstabilizedLocation =
  LocationWithoutInvestigators
    <> LocationWithoutEnemies
    <> not_ (LocationWithToken Token.Resource)

{- | Send a location's card back to the scanning deck and take the location out
of play. Deliberately not 'removeLocation': that diverts a victory location to
the victory display, but a location worth victory points only scores if it is
still in play at the end of the scenario, and this one has rejoined the deck.
-}
shuffleLocationIntoScanningDeck
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> m ()
shuffleLocationIntoScanningDeck (asId -> lid) = do
  card <- field LocationCard lid
  shuffleIntoScanningDeck [card]
  pushAll $ resolve (RemoveLocation lid)

shuffleEmptyUnstabilizedLocations :: ReverseQueue m => m ()
shuffleEmptyUnstabilizedLocations =
  selectEach emptyUnstabilizedLocation shuffleLocationIntoScanningDeck

{- | The printed front of The Quantum Maelstrom, identical on all three
printings:

"[action]: Scan. Search for the topmost card in the scanning deck with an icon
matching your current location and draw it. If it is a location, put it into
play and move to it. Shuffle the scanning deck."

Ability 2 defers the move until the scanned-card draw has had a chance to put
the location into play; ability 3 then performs it.
-}
quantumMaelstromAbilities :: AgendaAttrs -> [Ability]
quantumMaelstromAbilities a =
  [ restricted a 1 NoRestriction scanAction_
  , mkAbility a 2
      $ SilentForcedAbility
      $ CampaignEvent #after (Just You) (scanEventForCardType LocationType)
  ]

{- | The shared tail of all three printings of The Quantum Maelstrom:

"If there is another agenda below this one, set this agenda aside, out of play.
Otherwise, add 1 tally mark under \"Impending Doom\" in your Campaign Log and
shuffle this agenda with each set aside agenda to form a new agenda deck."

Set aside agendas are the printings this scenario has already cycled through,
so the reformed deck is always all three again.
-}
advanceQuantumMaelstrom :: (ReverseQueue m, IsCard card) => card -> m ()
advanceQuantumMaelstrom (toCard -> card) = do
  below <- filter ((/= toCardId card) . toCardId) <$> getAgendaDeckCards 1
  case below of
    [] -> do
      addImpendingDoom 1
      -- Read the set aside pile directly: 'getSetAsideCardsMatching' matches
      -- against the card registry, and the printings we set aside are the
      -- agendas' own card values, which do not compare equal to it.
      setAsideAgendas <- filter ((== AgendaType) . toCardType) <$> scenarioField ScenarioSetAsideCards
      -- SetCurrentAgendaDeck pulls the new deck back out of the set aside pool
      setCurrentAgendaDeck =<< shuffle (card : setAsideAgendas)
    rest -> do
      -- Not the lifted 'setCardAside': its 'obtainCard' would push ObtainCard
      -- for a card that is still the in-play agenda.
      push $ SetCardAside card
      setCurrentAgendaDeck rest

-- ** The Evidence deck (In the Shadow of Earth) ** --

{- | Ship Mainframe and Telecoms both print "Draw the top card of the 'Evidence'
deck and read it." The Evidence cards are story cards; drawing one resolves it
as a story and removes it from the deck.
-}
getEvidenceDeck :: HasGame m => m [Card]
getEvidenceDeck = getScenarioDeck EvidenceDeck

drawEvidence :: ReverseQueue m => InvestigatorId -> m ()
drawEvidence iid =
  getEvidenceDeck >>= \case
    [] -> pure ()
    (x : rest) -> do
      setScenarioDeck EvidenceDeck rest
      resolveStory iid x

-- ** Imitations and the crew of the Nostalgia II (In the Shadow of Earth) ** --

{- | Each "Evidence" story card vouches for exactly one [[Crew]] story asset
("<crew member> is not an imitation."). Act 2b and resolution 1 both reveal one
chaos token per story card hidden under the scenario reference card and, on a
bad token, mark that card's crew member as an imitation of the Entity.
-}
evidenceCrew :: [(CardDef, CardDef)]
evidenceCrew =
  [ (Stories.evidenceAdamTanner, Assets.adamTanner)
  , (Stories.evidenceCaptainBurr, Assets.captainBurr)
  , (Stories.evidenceDoctorFeng, Assets.doctorFeng)
  , (Stories.evidenceLtArcherMichaels, Assets.ltArcherMichaels)
  , (Stories.evidenceMUD12Mudbug, Assets.muD12Mudbug)
  , (Stories.evidenceSophie, Assets.sophie)
  ]

-- | The [[Crew]] story asset an "Evidence" story card corresponds to.
crewForEvidence :: HasCardDef a => a -> Maybe CardDef
crewForEvidence a = lookup (toCardDef a) evidenceCrew

{- | Guide p14 (resolution 1) and act 2b, "Quarantine": "For each of the story
cards, reveal 1 random chaos token from the chaos bag. If it is not a [elder
sign], [bless], '+1', or '0' token, the [[Crew]] story asset corresponding to
that story card is an imitation of the Entity!"
-}
clearsSuspicionTokens :: [ChaosTokenFace]
clearsSuspicionTokens = [ElderSign, BlessToken, PlusOne, Zero]

isImitationToken :: ChaosTokenFace -> Bool
isImitationToken = (`notElem` clearsSuspicionTokens)

{- | Guide p14, "Cards Removed from the Game": removed cards are kept in an
accessible out-of-play area. Airlocks' resign ability and the agenda 1-3 forced
ability both remove [[Crew]] story assets from the game, and resolution 3 counts
them. @RemoveFromGame@ deletes the asset entity, so the only trace left is the
card itself.
-}
getRemovedCrew :: HasGame m => m [Card]
getRemovedCrew = filterCards (CardWithTrait Crew) <$> getRemovedFromPlayCards

{- | [[Crew]] story assets attached facedown to The Entity — act 2b attaches the
removed and imitation crew there, and agenda 4a keeps doing so for the rest of
the scenario. Resolutions 3 and 4 count them.

Two attachment shapes have to be counted. Agenda 4a moves a crew member that is
still in play, so it survives as an asset with an @AttachedToEnemy@ placement.
Act 2b instead attaches crew that were removed from the game or are imitations
still sitting in the scanning deck; those have no asset entity, so they can only
be placed as cards underneath the enemy.
-}
getCrewAttachedToTheEntity :: HasGame m => m [Card]
getCrewAttachedToTheEntity = do
  entities <- select $ IncludeOutOfPlayEnemy $ enemyIs Enemies.theEntity
  concat <$> for entities \eid -> do
    assets <- select $ AssetWithTrait Crew <> AssetWithPlacement (AttachedToEnemy eid)
    attached <- traverse (field AssetCard) assets
    underneath <- field EnemyCardsUnderneath eid
    pure $ attached <> filterCards (CardWithTrait Crew) underneath

-- | [[Crew]] story assets never scanned up, still in the scanning deck.
getCrewInScanningDeck :: HasGame m => m [Card]
getCrewInScanningDeck = filterCards (CardWithTrait Crew) <$> getScanningDeck

{- | The broad @"switched"@ ScenarioEvent (Electric Nightmare): two locations
traded places. Only for cards that care about *any* switch.
-}
switchedEvent :: Text
switchedEvent = "switched"

{- | Narrower key for a card whose text names *its own* location — "After Glitch
in the System's location is switched…", "After Entrance Hall is switched…". A
handler-side check on the payload is too late: the ability is offered (or enters
the forced-trigger ordering) after every switch anywhere on the map, so the
condition has to live in the window key.
-}
switchedEventFor :: LocationId -> Text
switchedEventFor lid = switchedEvent <> "[" <> tshow lid <> "]"

{- | Narrower key for "After *your* location is switched…" — fired once per
investigator standing at either of the two locations, carrying that
investigator, so cards match with @ScenarioEvent #after (Just You)@. Their
location is not knowable when abilities are collected, so the location-keyed
variant above cannot serve them.
-}
switchedEventForInvestigator :: Text
switchedEventForInvestigator = switchedEvent <> "[investigator]"

{- | The window for a card that reacts to *its own* location being switched,
picked from where the card actually sits.

An enemy is only @AtLocation@ while it is unengaged; the moment it engages an
investigator its placement becomes 'InThreatArea' and there is no location id
left to key on. The per-investigator window covers that case exactly: it fires
for each investigator standing at either switched location, and an enemy in a
threat area is at its investigator's location by definition.

'Nothing' means the card is somewhere that cannot be switched, in which case it
should get no ability at all rather than one keyed to every switch on the map.
-}
switchedWindowFor :: Placement -> Maybe WindowMatcher
switchedWindowFor = \case
  AtLocation lid -> Just $ ScenarioEvent #after Nothing (switchedEventFor lid)
  AttachedToLocation lid -> Just $ ScenarioEvent #after Nothing (switchedEventFor lid)
  InThreatArea iid ->
    Just $ ScenarioEvent #after (Just $ InvestigatorWithId iid) switchedEventForInvestigator
  _ -> Nothing

-- | Is this window key @switched@ or one of its narrower @switched[...]@ companions?
isSwitchedEvent :: Text -> Bool
isSwitchedEvent key = key == switchedEvent || (switchedEvent <> "[") `isPrefixOf` key

{- | Announce a switch. Every key in the family carries the same payload — the
two locations that traded places — so a card can match the narrowest window that
fits its printed text and still read the details with 'getSwitchedLocations'.
-}
checkSwitchedWindows :: ReverseQueue m => LocationId -> LocationId -> m ()
checkSwitchedWindows a b = do
  iids <- select $ InvestigatorAt (mapOneOf LocationWithId [a, b])
  let payload = toJSON (a, b)
  checkWindows
    $ [ Window.mkAfter $ Window.ScenarioEvent key Nothing payload
      | key <- [switchedEvent, switchedEventFor a, switchedEventFor b]
      ]
    <> [ Window.mkAfter $ Window.ScenarioEvent switchedEventForInvestigator (Just iid) payload
       | iid <- iids
       ]

getSwitchedLocations :: [Window.Window] -> Maybe (LocationId, LocationId)
getSwitchedLocations = \case
  (Window.windowType -> Window.ScenarioEvent k _ v) : _ | isSwitchedEvent k -> Just (toResult v)
  _ : rest -> getSwitchedLocations rest
  [] -> Nothing

-- ** Flipping locations (Fragment of Carcosa) ** --

{- | Fragment of Carcosa's cave locations are double-sided: each has a Carcosa
face on its reverse ('cdOtherSide'). Flipping swaps the card in place, so the
location keeps its grid position, tokens and occupants. Locations without an
other side (the [[Surface]] ones, which print "Cannot be flipped.") are
silently left alone.
-}
flipToOtherSide :: ReverseQueue m => LocationAttrs -> m ()
flipToOtherSide attrs =
  for_ (toCardDef attrs).flip \other -> do
    let replace = ReplaceLocation attrs.id (lookupCard other attrs.cardId) Swap
    push replace
    -- Guide: "Then, add clues on that location up to its clue value." The new
    -- side's clue value is only readable once the swap has resolved, so the
    -- top-up is deferred; the Fragment of Carcosa scenario handles this step.
    push $ Msg.DoStep 1 replace

{- | "any [[Cave]] or [[Carcosa]] location" — the locations Fragment of Carcosa's
flip effects may choose. Restricted to those that actually have another side,
so a "cannot be flipped" location is never offered.
-}
caveOrCarcosaLocation :: LocationMatcher
caveOrCarcosaLocation =
  oneOf [LocationWithTrait Cave, LocationWithTrait Carcosa] <> LocationCanBeFlipped

{- | Acts II and III of Fragment of Carcosa both print "Flip your location and
all connecting locations to their other side." Locations that cannot be
flipped are skipped.
-}
flipSurroundingLocations
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
flipSurroundingLocations iid (toSource -> source) = do
  locations <-
    select $ oneOf [locationWithInvestigator iid, connectedTo (locationWithInvestigator iid)]
  flippable <- select LocationCanBeFlipped
  for_ (filter (`elem` flippable) locations) \lid -> push $ Flip iid source (toTarget lid)

-- ** Face-down encounter cards in a threat area (Lost Quantum) ** --

{- | Scenario IIIa puts encounter cards *face down* into investigators' threat
areas; they sit there unresolved, are counted by several cards, and are later
"drawn" — resolved as if just drawn. The zone is
'Placement.FacedownInThreatArea'; entities are created via @CreateTreacheryAt@
/ @createEnemyWithPlacement@, which build the entity **without** running its
revelation.
-}
facedownInThreatAreaOf :: InvestigatorId -> TreacheryMatcher
facedownInThreatAreaOf iid = TreacheryWithPlacement (FacedownInThreatArea iid)

getFacedownCards :: HasGame m => InvestigatorId -> m [TreacheryId]
getFacedownCards = select . facedownInThreatAreaOf

{- | Face-down *enemies*: 'placeFacedownInThreatArea' puts an encounter card of
either kind into the zone, so anything that counts or draws face-down cards has
to look at both.
-}
facedownEnemiesOf :: InvestigatorId -> EnemyMatcher
facedownEnemiesOf = EnemyWithPlacement . FacedownInThreatArea

getFacedownEnemies :: HasGame m => InvestigatorId -> m [EnemyId]
getFacedownEnemies = select . facedownEnemiesOf

facedownAssetsOf :: InvestigatorId -> AssetMatcher
facedownAssetsOf = AssetWithPlacement . FacedownInThreatArea

getFacedownAssets :: HasGame m => InvestigatorId -> m [AssetId]
getFacedownAssets = select . facedownAssetsOf

-- | Every face-down card in the threat area, regardless of its card type.
getFacedownCardCount :: HasGame m => InvestigatorId -> m Int
getFacedownCardCount iid =
  sum
    <$> sequence
      [ selectCount (facedownInThreatAreaOf iid)
      , selectCount (facedownEnemiesOf iid)
      , selectCount (facedownAssetsOf iid)
      ]

{- | "If you have N or more face-down encounter cards in your threat area", the
'Criterion' counterpart of 'getFacedownCardCount'. The zone spans three entity
types, so the total has to be summed in a calculation; 'TreacheryCount' and its
siblings each only see one of them.
-}
yourFacedownCardsAtLeast :: Int -> Criterion
yourFacedownCardsAtLeast n =
  HasCalculation
    ( SumCalculation
        [ CountTreacheries (TreacheryFacedownInThreatAreaOf You)
        , CountEnemies (EnemyFacedownInThreatAreaOf You)
        , CountAssets (AssetFacedownInThreatAreaOf You)
        ]
    )
    (atLeast n)

{- | "if there are face-down encounter cards in any investigator's threat area".
A trigger condition, so it has to be a 'Criterion': a 'Forced' ability that only
checks this in its handler is still offered — and a forced ability is a
mandatory click — every single time its window opens.
-}
anyFacedownEncounterCards :: Criterion
anyFacedownEncounterCards =
  oneOf
    [ exists $ TreacheryFacedownInThreatAreaOf Anyone
    , exists $ EnemyFacedownInThreatAreaOf Anyone
    , exists $ AssetFacedownInThreatAreaOf Anyone
    ]

-- | "Place the top card of the encounter deck into your threat area, face-down."
placeCardFacedownInThreatArea :: ReverseQueue m => InvestigatorId -> Card -> m ()
placeCardFacedownInThreatArea iid card = case toCardType card of
  EnemyType -> push =<< Msg.createEnemyWithPlacement_ card placement
  -- Erwin Simmons (Fading) is an @encounterAsset_@: EncounterAssetType, not
  -- AssetType. Without it here the card falls through to the treachery branch.
  cardType | cardType `elem` [AssetType, EncounterAssetType] -> createAssetAt_ card placement
  _ -> createTreacheryAt_ card placement
 where
  placement = FacedownInThreatArea iid

placeCardsFacedownEvenly :: ReverseQueue m => [InvestigatorId] -> [Card] -> m ()
placeCardsFacedownEvenly investigators cards = unless (null investigators) do
  shuffled <- shuffle cards
  for_ (zip shuffled $ cycleN (length shuffled) investigators) \(card, iid) ->
    placeCardFacedownInThreatArea iid card

-- | Remove the top @n@ cards of the encounter deck and hand them back.
takeTopOfEncounterDeck :: ReverseQueue m => Int -> m [Card]
takeTopOfEncounterDeck n =
  getEncounterDeck >>= \case
    Deck (splitAt n -> (cards, rest)) -> do
      setEncounterDeck (Deck rest)
      pure $ map toCard cards

{- | @doPlaceFacedown@ — the tail of a placement that ran the encounter deck dry
and is waiting on 'shuffleEncounterDiscardBackIn'. The reshuffle is a queued
message, so the cards still owed can only be taken a step later;
"Arkham.Homebrew.DarkMatter.Campaign" resolves this for every scenario, exactly
as it resolves 'doScanKey'. Payload is the investigator and the number of cards
still owed.
-}
doPlaceFacedownKey :: Text
doPlaceFacedownKey = "doPlaceFacedown"

{- | "Put the top card(s) of the encounter deck into your threat area, face-down."

Rules Reference, Encounter Deck: "If the encounter deck is empty, shuffle the
encounter discard pile back into the encounter deck." A short deck therefore
places what it has, reshuffles, and comes back for the rest — mirroring the
engine's own partial-draw loop in "Arkham.Scenario.Runner". With both deck and
discard empty nothing is placed, and Lost Quantum's face-down rule takes over.
-}
placeFacedownInThreatArea :: ReverseQueue m => InvestigatorId -> Int -> m ()
placeFacedownInThreatArea iid n = do
  cards <- takeTopOfEncounterDeck n
  traverse_ (placeCardFacedownInThreatArea iid) cards
  let owed = n - length cards
  when (owed > 0) do
    discardPile <- scenarioField ScenarioDiscard
    unless (null discardPile) do
      shuffleEncounterDiscardBackIn
      push $ CampaignSpecific doPlaceFacedownKey (toJSON (iid, owed))

{- | Guard for an ability whose cost is "put the top card of the encounter deck
into your threat area, face-down". 'EncounterDeckIsNotEmpty' alone is too strict:
the placement reshuffles the discard, so only an empty deck *and* an empty
discard makes the cost unpayable.
-}
canPlaceFacedownInThreatArea :: Criterion
canPlaceFacedownInThreatArea =
  oneOf
    [ EncounterDeckIsNotEmpty
    , HasCalculation (ScenarioInDiscardCountCalculation AnyCard) (atLeast 1)
    ]

{- | The 'HasGame' twin of 'canPlaceFacedownInThreatArea', for effects that offer
the placement as one of several "you must (choose one)" options: an option with
no potential to change the game state may not be offered at all.
-}
getCanPlaceFacedownInThreatArea :: HasGame m => m Bool
getCanPlaceFacedownInThreatArea =
  orM [notNull . unDeck <$> getEncounterDeck, notNull <$> scenarioField ScenarioDiscard]

{- | "Draw a face-down encounter card in your threat area" — the card leaves the
face-down zone and resolves as if just drawn.
-}
drawFacedownCard :: ReverseQueue m => InvestigatorId -> TreacheryId -> m ()
drawFacedownCard iid tid = drawFacedownCardWith iid tid (pure ())

{- | 'drawFacedownCard' with an extra step that resolves once the card is face
up but before its revelation is initiated.

Destabilization prints "If it is a treachery, you may spend 1 clue to cancel
its revelation effect": cancelling is the 'IgnoreRevelation' modifier, which
'ResolveTreachery' reads when it runs, so the offer has to land between the
flip and the resolve.
-}
drawFacedownCardWith :: ReverseQueue m => InvestigatorId -> TreacheryId -> m () -> m ()
drawFacedownCardWith iid tid afterFlip = do
  -- Back to the placement a freshly-created treachery has, so its revelation
  -- resolves exactly as if it had just been drawn.
  push $ PlaceTreachery tid Limbo
  checkFacedownDrawnWindows iid tid
  afterFlip
  -- ResolveTreachery, not a bare Revelation: it is the engine's "resolve this
  -- treachery entity as if just drawn" entry point, so it wraps the revelation
  -- in its #when/#after windows, discards (or claims the victory for) the
  -- treachery once it is done, marks the card resolved, and — crucially here —
  -- honours IgnoreRevelation by discarding the treachery unresolved instead.
  push $ ResolveTreachery iid tid

{- | Payload is the id of the entity that was just drawn out of the face-down
zone. It can be a 'TreacheryId', an 'EnemyId' or an 'AssetId', so no consumer may
assume a type from the base key alone.
-}
facedownDrawnEvent :: Text
facedownDrawnEvent = "drewFacedown"

{- | @drewFacedown[<entity id>]@. Quantum Collapse prints "After you draw Quantum
Collapse from your threat area", which is a condition on *which* card was drawn —
that has to live in the window key, not in the handler, or every face-down copy
is offered a forced ability on every face-down draw and the player is asked to
order triggers that all no-op. See the bracketed-key family @checkScanWindows@
uses.
-}
facedownDrawnEventFor :: Show a => a -> Text
facedownDrawnEventFor entityId = facedownDrawnEvent <> "[" <> tshow entityId <> "]"

{- | Fire the whole family in one 'checkWindows' call so a reaction to the narrow
key is simultaneous with one to the base key rather than strictly after it.
-}
checkFacedownDrawnWindows
  :: (ReverseQueue m, Show a, ToJSON a) => InvestigatorId -> a -> m ()
checkFacedownDrawnWindows iid entityId =
  checkWindows
    $ map
      (\key -> Window.mkAfter $ Window.ScenarioEvent key (Just iid) (toJSON entityId))
      [facedownDrawnEvent, facedownDrawnEventFor entityId]

{- | Is this window key @drewFacedown@ or one of its narrower companions? Every
key in the family carries the same payload, so a card that moves to a narrow key
can still read the drawn entity's id.
-}
isFacedownDrawnEvent :: Text -> Bool
isFacedownDrawnEvent key =
  key == facedownDrawnEvent || (facedownDrawnEvent <> "[") `isPrefixOf` key

-- | Draw every face-down card in a threat area, one at a time.

{- | Drawing a face-down *enemy* resolves it exactly as a freshly drawn enemy:
'InvestigatorDrawEnemy' spawns it out of the face-down zone, then its revelation
runs. Mirrors the encounter-draw path in @Arkham.Game.Runner@.
-}
drawFacedownEnemy :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
drawFacedownEnemy iid eid = do
  checkFacedownDrawnWindows iid eid
  Msg.pushAll [InvestigatorDrawEnemy iid eid, Revelation iid (EnemySource eid)]

{- | Encounter assets such as Erwin Simmons can also be among the face-down
cards. Remove the hidden asset entity, then resolve its card as a fresh
encounter draw so its revelation creates the normal in-play asset.
-}
drawFacedownAsset :: ReverseQueue m => InvestigatorId -> AssetId -> m ()
drawFacedownAsset iid aid = do
  card <- field AssetCard aid
  checkFacedownDrawnWindows iid aid
  removeAsset aid
  push $ Revelation iid (CardIdSource card.id)

data FacedownEncounterCard
  = FacedownTreachery TreacheryId
  | FacedownEnemy EnemyId
  | FacedownAsset AssetId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getFacedownEncounterCards :: HasGame m => InvestigatorId -> m [FacedownEncounterCard]
getFacedownEncounterCards iid =
  concat
    <$> sequence
      [ map FacedownTreachery <$> getFacedownCards iid
      , map FacedownEnemy <$> getFacedownEnemies iid
      , map FacedownAsset <$> getFacedownAssets iid
      ]

drawFacedownEncounterCard :: ReverseQueue m => InvestigatorId -> FacedownEncounterCard -> m ()
drawFacedownEncounterCard iid = \case
  FacedownTreachery tid -> drawFacedownCard iid tid
  FacedownEnemy eid -> drawFacedownEnemy iid eid
  FacedownAsset aid -> drawFacedownAsset iid aid

placeFacedownEncounterCard :: ReverseQueue m => InvestigatorId -> FacedownEncounterCard -> m ()
placeFacedownEncounterCard iid = \case
  FacedownTreachery tid -> Placement.place tid placement
  FacedownEnemy eid -> Placement.place eid placement
  FacedownAsset aid -> Placement.place aid placement
 where
  placement = FacedownInThreatArea iid

{- | The entity-level counterpart of 'placeCardsFacedownEvenly': shuffle cards
that are already in play (or already face-down) and deal them back out
round-robin, "as evenly as possible".
-}
placeFacedownEncounterCardsEvenly
  :: ReverseQueue m => [InvestigatorId] -> [FacedownEncounterCard] -> m ()
placeFacedownEncounterCardsEvenly investigators cards = unless (null investigators) do
  shuffled <- shuffle cards
  for_ (zip shuffled $ cycleN (length shuffled) investigators) \(card, iid) ->
    placeFacedownEncounterCard iid card

-- | Randomly draw one face-down encounter card. Returns whether a card existed.
drawRandomFacedownCard :: ReverseQueue m => InvestigatorId -> m Bool
drawRandomFacedownCard iid = drawRandomFacedownCardWith iid (const $ pure ())

{- | 'drawRandomFacedownCard' with an extra step, run only when the drawn card is
a treachery, between the flip and its revelation. See 'drawFacedownCardWith'.
-}
drawRandomFacedownCardWith
  :: ReverseQueue m => InvestigatorId -> (TreacheryId -> m ()) -> m Bool
drawRandomFacedownCardWith iid afterFlip = do
  cards <- getFacedownEncounterCards iid
  for_ (nonEmpty cards) $ sample >=> \case
    FacedownTreachery tid -> drawFacedownCardWith iid tid (afterFlip tid)
    card -> drawFacedownEncounterCard iid card
  pure $ not (null cards)

{- | @doDrawFacedown@ — the face-down cards a "one at a time" draw still owes.
The cards to draw are picked up front, but the draws cannot all be queued up
front: a drawn card can itself draw the rest of the zone (Quantum Collapse does
exactly that), and re-drawing a card that has since resolved and been discarded
throws @MissingEntity@ out of 'ResolveTreachery'. Each draw therefore hands what
is left back to "Arkham.Homebrew.DarkMatter.Campaign", exactly as 'doScanKey' and
'doPlaceFacedownKey' do, so the next card is only reached once the previous one
has fully resolved.
-}
doDrawFacedownKey :: Text
doDrawFacedownKey = "doDrawFacedown"

{- | Draw the given face-down cards, one at a time, skipping any that something
else has drawn out of the zone in the meantime.
-}
drawFacedownEncounterCards
  :: ReverseQueue m => InvestigatorId -> [FacedownEncounterCard] -> m ()
drawFacedownEncounterCards iid = \case
  [] -> pure ()
  card : rest -> do
    stillFacedown <- elem card <$> getFacedownEncounterCards iid
    when stillFacedown $ drawFacedownEncounterCard iid card
    unless (null rest) $ push $ CampaignSpecific doDrawFacedownKey (toJSON (iid, rest))

{- | "Draw @n@ face-down cards from your threat area." The cards are picked up
front, and distinct: every draw here is only queued, so sampling once per draw
would keep reading the same untouched threat area and could pick one card twice.
-}
drawFacedownCards :: ReverseQueue m => InvestigatorId -> Int -> m ()
drawFacedownCards iid n =
  getFacedownEncounterCards iid >>= sampleListN n >>= drawFacedownEncounterCards iid

drawAllFacedownCards :: ReverseQueue m => InvestigatorId -> m ()
drawAllFacedownCards iid = getFacedownEncounterCards iid >>= drawFacedownEncounterCards iid

-- ** The [[Avatar]] children (Public School 187) ** --

{- | Alma, David, Tilde and William each print "If The BOOGEYMAN is at the
location above or below <name>'s location", i.e. directly above or below on the
grid — not connected, and not the same location.
-}
boogeymanAboveOrBelow :: AssetId -> Criterion
boogeymanAboveOrBelow aid =
  exists
    $ mapOneOf (\d -> LocationInDirection d (locationWithAsset aid)) [Above, Below]
    <> LocationWithEnemy (enemyIs Enemies.theBOOGEYMAN)
