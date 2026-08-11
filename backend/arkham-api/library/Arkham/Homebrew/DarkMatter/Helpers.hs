module Arkham.Homebrew.DarkMatter.Helpers where

import Arkham.Ability
import Arkham.Actions (Actions (..))
import Arkham.Asset.Types (Field (AssetPlacement))
import Arkham.CampaignLog (campaignLogRecordedCounts)
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select, selectCount)
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario (getEncounterDeck, getScenarioDeck)
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorLog))
import Arkham.LocationSymbol
import Arkham.Matcher (AssetMatcher (AssetWithTrait), CardMatcher (AnyCard), TreacheryMatcher (..))
import Arkham.Message (
  Message (
    DrewCards,
    IncrementRecordCountForInvestigator,
    PlaceTreachery,
    Revelation,
    ShuffleCardsIntoDeck
  ),
  ShuffleIn (..),
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Setup
import Arkham.Source
import Arkham.Window qualified as Window

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "darkMatter" a

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

{- | Payload of the @"scan"@ 'Window.ScenarioEvent' fired after every scan,
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

scanEvent :: Text
scanEvent = "scan"

{- | Perform a scan for the given icon(s). A card matches only if it shows
every requested icon (Strange Moons' "Brain Scanning" scans for two icons;
a normal scan passes one). Non-matching cards are set aside face down and
shuffled back in afterwards; the first matching card is drawn. If no card
matches, the scan is unsuccessful.
-}
scan
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> [LocationSymbol] -> m ()
scan iid (toSource -> source) icons = scanWith iid icons (drawScannedCard iid source)

{- | 'scan' with a caller-supplied resolution for the matching card. Strange
Moons' Dream Diagnostics and Memory Scanner put a scanned *location* into play
on top of Reality Simulator rather than drawing it.
-}
scanWith
  :: ReverseQueue m => InvestigatorId -> [LocationSymbol] -> (Card -> m ()) -> m ()
scanWith iid icons onFound = do
  deck <- getScanningDeck
  let matches c = all (`elem` scanIcons c) icons
  case break matches deck of
    (skipped, []) -> do
      unless (null skipped) $ setScenarioDeck ScanningDeck =<< shuffle skipped
      checkAfter $ Window.ScenarioEvent scanEvent (Just iid) (toJSON $ ScanResult iid icons Nothing False)
    (skipped, x : rest) -> do
      deck' <- if null skipped then pure rest else shuffle (skipped <> rest)
      setScenarioDeck ScanningDeck deck'
      onFound x
      checkAfter $ Window.ScenarioEvent scanEvent (Just iid) (toJSON $ ScanResult iid icons (Just x) True)

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
      checkAfter $ Window.ScenarioEvent scanEvent (Just iid) (toJSON $ ScanResult iid [] Nothing False)
    (x : rest) -> do
      setScenarioDeck ScanningDeck rest
      drawScannedCard iid source x
      checkAfter
        $ Window.ScenarioEvent scanEvent (Just iid) (toJSON $ ScanResult iid (scanIcons x) (Just x) True)

drawScannedCard :: ReverseQueue m => InvestigatorId -> Source -> Card -> m ()
drawScannedCard iid source card = do
  focusCards [card] $ chooseTargetM iid [card] \_ -> unfocusCards
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

{- | "If such a situation arises that you would need to discard a card with
the scanning back or shuffle it into any other deck, shuffle it back into
the scanning deck instead."
-}
shuffleIntoScanningDeck :: (ReverseQueue m, IsCard card) => [card] -> m ()
shuffleIntoScanningDeck cards =
  push $ ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey ScanningDeck) (map toCard cards)

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

getFacedownCardCount :: HasGame m => InvestigatorId -> m Int
getFacedownCardCount = selectCount . facedownInThreatAreaOf

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
drawFacedownCard iid tid = do
  -- Back to the placement a freshly-created treachery has, so its revelation
  -- resolves exactly as if it had just been drawn.
  push $ PlaceTreachery tid Limbo
  checkAfter $ Window.ScenarioEvent facedownDrawnEvent (Just iid) (toJSON tid)
  push $ Revelation iid (TreacherySource tid)

{- | Payload is the 'TreacheryId' that was just drawn out of the face-down zone;
cards that care which card was drawn (Quantum Collapse) match on it.
-}
facedownDrawnEvent :: Text
facedownDrawnEvent = "drewFacedown"

-- | Draw every face-down card in a threat area, one at a time.
drawAllFacedownCards :: ReverseQueue m => InvestigatorId -> m ()
drawAllFacedownCards iid = getFacedownCards iid >>= traverse_ (drawFacedownCard iid)
