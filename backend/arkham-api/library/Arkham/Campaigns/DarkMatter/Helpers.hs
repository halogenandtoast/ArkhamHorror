module Arkham.Campaigns.DarkMatter.Helpers where

import Arkham.Ability
import Arkham.Campaigns.DarkMatter.Key
import Arkham.CampaignLog (campaignLogRecordedCounts)
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorLog))
import Arkham.LocationSymbol
import Arkham.Matcher (CardMatcher (AnyCard))
import Arkham.Message (
  Message (DrewCards, IncrementRecordCountForInvestigator, ShuffleCardsIntoDeck),
  ShuffleIn (..),
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Setup
import Arkham.Source
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "darkMatter" a

-- ** Memories (guide p3) ** --

-- "Memories" are tied to specific investigators and are not shared. They live
-- as a tally in each investigator's own campaign log section.

getMemories :: (HasGame m, Tracing m) => InvestigatorId -> m Int
getMemories =
  fieldMap InvestigatorLog (findWithDefault 0 (toCampaignLogKey Memories) . campaignLogRecordedCounts)

addMemories :: ReverseQueue m => InvestigatorId -> Int -> m ()
addMemories iid n = push $ IncrementRecordCountForInvestigator iid (toCampaignLogKey Memories) n

-- | Cross out tally marks. The number of Memories never drops below zero.
crossOffMemories :: ReverseQueue m => InvestigatorId -> Int -> m ()
crossOffMemories iid n = addMemories iid (negate n)

-- ** Impending Doom ** --

getImpendingDoom :: (HasGame m, Tracing m) => m Int
getImpendingDoom = getRecordCount ImpendingDoom

addImpendingDoom :: ReverseQueue m => Int -> m ()
addImpendingDoom = incrementRecordCount ImpendingDoom

-- ** Desynchronization (guide p6) ** --

-- | Scenario II intro: each investigator with 3 or fewer Memories must read
-- Desynchronization and add the Desync weakness to their deck (it does not
-- count towards the deck limit).
checkDesynchronization :: ReverseQueue m => m ()
checkDesynchronization = do
  iids <- filterM (fmap (<= 3) . getMemories) =<< getInvestigators
  unless (null iids) do
    campaignI18n $ scope "desynchronization" $ flavor $ setTitle "title" >> p "body"
    for_ iids \iid -> addCampaignCardToDeck iid ShuffleIn Treacheries.desyncDarkMatter

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

-- | Setup: "Create the scanning deck. This is done by taking all the (other)
-- encounter cards with icons at the bottom of their back side and shuffling
-- them together." Call after setting aside any scanning-back cards that the
-- setup excludes (they are gone from the gathered pool by then).
addScanningDeck :: ReverseQueue m => ScenarioBuilderT m ()
addScanningDeck = do
  cards <- filter hasScanningBack <$> amongGathered AnyCard
  removeCards cards
  addExtraDeck ScanningDeck =<< shuffle cards

getScanningDeck :: (HasGame m, Tracing m) => m [Card]
getScanningDeck = getScenarioDeck ScanningDeck

-- | The Scan action designator; usually, but not always, initiated using the
-- "activate" action.
scanAction :: Cost -> AbilityType
scanAction cost = ActionAbility #scan Nothing (ActionCost 1 <> cost)

scanAction_ :: AbilityType
scanAction_ = scanAction mempty

-- | Payload of the @"scan"@ 'Window.ScenarioEvent' fired after every scan,
-- successful or not.
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

-- | Perform a scan for the given icon(s). A card matches only if it shows
-- every requested icon (Strange Moons' "Brain Scanning" scans for two icons;
-- a normal scan passes one). Non-matching cards are set aside face down and
-- shuffled back in afterwards; the first matching card is drawn. If no card
-- matches, the scan is unsuccessful.
scan
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> [LocationSymbol] -> m ()
scan iid (toSource -> source) icons = do
  deck <- getScanningDeck
  let matches c = all (`elem` scanIcons c) icons
  case break matches deck of
    (skipped, []) -> do
      unless (null skipped) $ setScenarioDeck ScanningDeck =<< shuffle skipped
      checkAfter $ Window.ScenarioEvent scanEvent (Just iid) (toJSON $ ScanResult iid icons Nothing False)
    (skipped, x : rest) -> do
      deck' <- if null skipped then pure rest else shuffle (skipped <> rest)
      setScenarioDeck ScanningDeck deck'
      drawScannedCard iid source x
      checkAfter $ Window.ScenarioEvent scanEvent (Just iid) (toJSON $ ScanResult iid icons (Just x) True)

-- | Motion scanning (In the Shadow of Earth): simply draw the top card of the
-- scanning deck. The caller is responsible for the "only while at a location
-- with a matching icon" restriction.
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

-- | "If such a situation arises that you would need to discard a card with
-- the scanning back or shuffle it into any other deck, shuffle it back into
-- the scanning deck instead."
shuffleIntoScanningDeck :: (ReverseQueue m, IsCard card) => [card] -> m ()
shuffleIntoScanningDeck cards =
  push $ ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey ScanningDeck) (map toCard cards)
