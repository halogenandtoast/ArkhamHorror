module Arkham.Homebrew.DarkMatter.Helpers where

import Arkham.Ability
import Arkham.Actions (Actions (..))
import Arkham.Asset.Types (Field (AssetCard, AssetPlacement))
import Arkham.CampaignLog (campaignLogRecordedCounts)
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosTokenFace (..))
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select, selectAny, selectCount, selectOne, selectWithField)
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Enemy.Types (Field (EnemyCardsUnderneath))
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv (getCurrentBatchId)
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Game (getRemovedFromPlayCards)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Helpers.Scenario (getEncounterDeck, getScenarioDeck)
import Arkham.Helpers.Window (wouldWindows)
import Arkham.Helpers.Xp
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern EvidenceDeck, pattern ScanningDeck)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain, pattern Carcosa)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorLog, InvestigatorMentalTrauma))
import Arkham.Location.Types (LocationAttrs)
import Arkham.LocationSymbol
import Arkham.Matcher (
  AssetMatcher (AssetWithPlacement, AssetWithTrait),
  CardMatcher (AnyCard, CardWithTrait),
  EnemyMatcher (EnemyWithPlacement, IncludeOutOfPlayEnemy),
  InvestigatorMatcher (InvestigatorCanGainXp),
  LocationMatcher (LocationCanBeFlipped, LocationWithTitle, LocationWithTrait),
  TreacheryMatcher (..),
  assetIs,
  connectedTo,
  enemyIs,
  locationWithInvestigator,
  oneOf,
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
    ShuffleCardsIntoDeck,
    StoryMessage,
    Would
  ),
  ReplaceStrategy (Swap),
  ShuffleIn (..),
  pattern InvestigatorDrawEnemy,
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Story (resolveStory)
import Arkham.Message.Story (StoryMessage (RemoveStory))
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Setup
import Arkham.Source
import Arkham.Story.Types (StoryAttrs)
import Arkham.Target
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

-- ** Desynchronization (guide p6) ** --

{- | Scenario II intro: each investigator with 3 or fewer Memories must read
Desynchronization and add the Desync weakness to their deck (it does not
count towards the deck limit).
-}
checkDesynchronization :: ReverseQueue m => m ()
checkDesynchronization = do
  iids <- filterM (fmap (<= 3) . getMemories) =<< getInvestigators
  unless (null iids) do
    campaignI18n $ scope "desynchronization" $ flavor $ setTitle "title" >> p "body"
    for_ iids \iid -> addCampaignCardToDeck iid ShuffleIn Treacheries.desync

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

{- | The [[Brain]] story assets currently attached to a location. Strange Moons'
[[Interface]] locations scan for their own icon plus the icon of a brain
attached to them.
-}
brainsAttachedTo :: HasGame m => LocationId -> m [AssetId]
brainsAttachedTo lid = do
  assets <- select $ AssetWithTrait Brain
  filterM (fmap (== AttachedToLocation lid) . field AssetPlacement) assets

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

{- | Payload of the @"scan"@ 'Window.CampaignEvent' fired after every scan,
successful or not.
-}
data ScanResult = ScanResult
  { scannedBy :: InvestigatorId
  , scannedFor :: [LocationSymbol]
  , scannedCard :: Maybe Card
  , scanSuccessful :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Fired after every scan, successful or not. Scanning is campaign-wide, so
this is a 'Window.CampaignEvent' like 'wouldScanEvent'.
-}
scanEvent :: Text
scanEvent = "scan"

{- | Per-icon companion to 'scanEvent': a scan for [Trefoil] also fires
@scan[Trefoil]@. Cards that care about one icon match that window directly
instead of triggering on every scan and re-checking the payload — the same
bracketed-key convention the Scarlet Keys concealed cards use
(@noConcealed[<kind>]@).
-}
scanEventFor :: LocationSymbol -> Text
scanEventFor icon = scanEvent <> "[" <> tshow icon <> "]"

{- | Announce a finished scan: the general window, then one window per icon
scanned for.
-}
checkScanWindows :: ReverseQueue m => ScanResult -> m ()
checkScanWindows r = do
  checkAfter $ Window.CampaignEvent scanEvent (Just $ scannedBy r) (toJSON r)
  for_ (ordNub $ scannedFor r) \icon ->
    checkAfter $ Window.CampaignEvent (scanEventFor icon) (Just $ scannedBy r) (toJSON r)

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
  let matches c = all (`elem` scanIcons c) icons
  case break matches deck of
    (skipped, []) -> do
      unless (null skipped) $ setScenarioDeck ScanningDeck =<< shuffle skipped
      checkScanWindows $ ScanResult iid icons Nothing False
    (skipped, x : rest) -> do
      deck' <- if null skipped then pure rest else shuffle (skipped <> rest)
      setScenarioDeck ScanningDeck deck'
      drawScannedCard iid source x
      checkScanWindows $ ScanResult iid icons (Just x) True

{- | Motion scanning (In the Shadow of Earth): simply draw the top card of the
scanning deck. The caller is responsible for the "only while at a location
with a matching icon" restriction.
-}
scanTopOfScanningDeck
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
scanTopOfScanningDeck iid (toSource -> source) = do
  deck <- getScanningDeck
  case deck of
    [] ->
      checkScanWindows $ ScanResult iid [] Nothing False
    (x : rest) -> do
      setScenarioDeck ScanningDeck rest
      drawScannedCard iid source x
      checkScanWindows $ ScanResult iid (scanIcons x) (Just x) True

{- | Draw a scanned card. A scanned *location* is put into play on top of Reality
Simulator instead — that location prints "(Reminder - Reality Simulator is not in
play while there is a card on top of it)", and Dream Diagnostics and Memory
Scanner are the only things that can scan one up.
-}
drawScannedCard :: ReverseQueue m => InvestigatorId -> Source -> Card -> m ()
drawScannedCard iid source card | toCardType card == LocationType = do
  simulator <- selectOne $ LocationWithTitle "Reality Simulator"
  case simulator of
    Just lid -> push $ ReplaceLocation lid card Swap
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
cards, reveal 1 random chaos token from the chaos bag. If it is not a [skull],
[tablet], '+1', or '0' token, the [[Crew]] story asset corresponding to that
story card is an imitation of the Entity!"

NOTE: the extracted campaign guide reads [tablet] here; the hand transcription
this was implemented from read [cultist] instead. The guide spells [icon:
cultist] out correctly in three other resolutions, so [tablet] is taken as
authoritative. If that turns out to be wrong, this list is the only thing that
has to change.
-}
clearsSuspicionTokens :: [ChaosTokenFace]
clearsSuspicionTokens = [Skull, Tablet, PlusOne, Zero]

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

{- | Payload of the @"switched"@ ScenarioEvent (Electric Nightmare): the two
locations that traded places. Cards whose text names *their own* location —
"After Glitch in the System's location is switched…" — must check this, or they
fire on every switch anywhere on the map.
-}
switchedEvent :: Text
switchedEvent = "switched"

getSwitchedLocations :: [Window.Window] -> Maybe (LocationId, LocationId)
getSwitchedLocations = \case
  (Window.windowType -> Window.ScenarioEvent k _ v) : _ | k == switchedEvent -> Just (toResult v)
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

-- | Every face-down card in the threat area, treacheries and enemies alike.
getFacedownCardCount :: HasGame m => InvestigatorId -> m Int
getFacedownCardCount iid =
  (+) <$> selectCount (facedownInThreatAreaOf iid) <*> selectCount (facedownEnemiesOf iid)

-- | "Place the top card of the encounter deck into your threat area, face-down."
placeFacedownInThreatArea :: ReverseQueue m => InvestigatorId -> Int -> m ()
placeFacedownInThreatArea iid n = replicateM_ n do
  getEncounterDeck >>= \case
    Deck [] -> pure ()
    Deck (card : rest) -> do
      setEncounterDeck (Deck rest)
      let c = toCard card
      if toCardType c == EnemyType
        then push =<< Msg.createEnemyWithPlacement_ c (FacedownInThreatArea iid)
        else createTreacheryAt_ c (FacedownInThreatArea iid)

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
  checkAfter $ Window.ScenarioEvent facedownDrawnEvent (Just iid) (toJSON tid)
  afterFlip
  -- ResolveTreachery, not a bare Revelation: it is the engine's "resolve this
  -- treachery entity as if just drawn" entry point, so it wraps the revelation
  -- in its #when/#after windows, discards (or claims the victory for) the
  -- treachery once it is done, marks the card resolved, and — crucially here —
  -- honours IgnoreRevelation by discarding the treachery unresolved instead.
  push $ ResolveTreachery iid tid

{- | Payload is the 'TreacheryId' that was just drawn out of the face-down zone;
cards that care which card was drawn (Quantum Collapse) match on it.
-}
facedownDrawnEvent :: Text
facedownDrawnEvent = "drewFacedown"

-- | Draw every face-down card in a threat area, one at a time.

{- | Drawing a face-down *enemy* resolves it exactly as a freshly drawn enemy:
'InvestigatorDrawEnemy' spawns it out of the face-down zone, then its revelation
runs. Mirrors the encounter-draw path in @Arkham.Game.Runner@.
-}
drawFacedownEnemy :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
drawFacedownEnemy iid eid = do
  checkAfter $ Window.ScenarioEvent facedownDrawnEvent (Just iid) (toJSON eid)
  Msg.pushAll [InvestigatorDrawEnemy iid eid, Revelation iid (EnemySource eid)]

drawAllFacedownCards :: ReverseQueue m => InvestigatorId -> m ()
drawAllFacedownCards iid = do
  getFacedownCards iid >>= traverse_ (drawFacedownCard iid)
  getFacedownEnemies iid >>= traverse_ (drawFacedownEnemy iid)
