module Arkham.Campaigns.EdgeOfTheEarth.Helpers (
  module Arkham.Campaigns.EdgeOfTheEarth.Helpers,
  module Arkham.Campaigns.EdgeOfTheEarth.Partner,
) where

import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Partner
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Capability
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Difficulty
import Arkham.Draw.Types
import Arkham.EncounterSet (EncounterSet (Tekelili))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Campaign
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (..))
import Arkham.Message (Message (DrawCards, SetPartnerStatus), ShuffleIn (..))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.PlayerCard
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Setup
import Arkham.Source
import Arkham.Treachery.Types (Field (TreacheryCardId))
import Arkham.Window (WindowType (ScenarioEvent))

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "edgeOfTheEarth" a

-- ** Shelter Helpers ** --

shelterValue :: (HasGame m, AsId location, IdOf location ~ LocationId) => location -> m (Maybe Int)
shelterValue location = do
  card <- field LocationCard (asId location)
  pure $ lookup "shelter" (toCardDef card).meta >>= maybeResult

getShelterValue :: HasCardCode a => a -> Maybe Int
getShelterValue a = do
  def <- lookupCardDef (toCardCode a)
  val <- lookup "shelter" (cdMeta def)
  maybeResult val

-- ** Supply Helpers ** --

whenRecoveredSupply :: HasGame m => Supply -> m () -> m ()
whenRecoveredSupply supply action = whenM (hasSupply supply) action

hasSupply :: HasGame m => Supply -> m Bool
hasSupply supply = inRecordSet (toJSON supply) SuppliesRecovered

recoverSupply :: ReverseQueue m => Supply -> m ()
recoverSupply supply = recordSetInsert SuppliesRecovered [toJSON supply]

toSupply :: HasCardCode a => a -> Supply
toSupply (toCardCode -> cardCode) = fromMaybe (error "missingSupply") $ find ((== cardCode) . toCardCode) [GreenSoapstone ..]

-- ** Tekeli-li Helpers ** --

getTekelili :: HasGame m => Int -> m [Card]
getTekelili n = take n <$> getScenarioDeck TekeliliDeck

addTekeliliDeck :: ReverseQueue m => ScenarioBuilderT m ()
addTekeliliDeck = addExtraDeck TekeliliDeck =<< shuffle =<< gatherTekelili

gatherTekelili :: (HasGame m, CardGen m) => m [Card]
gatherTekelili = do
  storyCards <- concat . toList <$> getCampaignStoryCards
  let storyCardDefs = map toCardDef storyCards
  let filteredDefs = foldl' (flip (deleteFirstMatch . (==))) defs storyCardDefs
  traverse genCard filteredDefs
 where
  defs =
    concatMap (\def -> replicate (fromMaybe 0 (cdEncounterSetQuantity def)) def)
      $ filter ((== Just Tekelili) . cdEncounterSet)
      $ toList allPlayerCards

addTekelili :: ReverseQueue m => InvestigatorId -> [Card] -> m ()
addTekelili _ [] = pure ()
addTekelili iid cards = whenM (can.manipulate.deck iid) do
  batched \batchId -> do
    checkWhen $ ScenarioEvent "shuffleTekelili" (Just iid) (toJSON (batchId, cards))
    traverse_ (addCampaignCardToDeck iid ShuffleIn) cards

resolveTekelili
  :: (ReverseQueue m, AsId tekelili, IdOf tekelili ~ TreacheryId) => InvestigatorId -> tekelili -> m ()
resolveTekelili iid tekelili = do
  cardId <- field TreacheryCardId (asId tekelili)
  mods <- getModifiers cardId
  if
    | PlaceOnBottomOfDeckInsteadOfDiscard `elem` mods -> putOnBottomOfDeck iid iid (asId tekelili)
    | ShuffleIntoDeckInsteadOfDiscard `elem` mods -> shuffleIntoDeck iid (asId tekelili)
    | ShuffleIntoAnyDeckInsteadOfDiscard `elem` mods -> do
        investigators <- getInvestigators
        chooseTargetM iid investigators \iid' -> shuffleIntoDeck iid' (asId tekelili)
    | LeaveCardWhereItIs `elem` mods -> pure ()
    | otherwise -> putOnBottomOfDeck iid TekeliliDeck (asId tekelili)

drawTekelili :: (Sourceable source, ReverseQueue m) => InvestigatorId -> source -> Int -> m ()
drawTekelili iid source n = push $ DrawCards iid $ newCardDraw source TekeliliDeck n

setPartnerStatus :: (HasCallStack, HasCardCode a, ReverseQueue m) => a -> PartnerStatus -> m ()
setPartnerStatus a = push . SetPartnerStatus (toPartnerCode a)

eliminatePartner :: (HasCallStack, HasCardCode a, ReverseQueue m) => a -> m ()
eliminatePartner = (`setPartnerStatus` Eliminated)

{- FOURMOLU_DISABLE -}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy -> [#"+1", #"+1", #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
  Standard -> [#"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-4", #frost, Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
  Hard -> [#"0", #"0", #"-1", #"-1", #"-2", #"-2", #"-3", #"-4", #"-4", #"-5", #frost, #frost, Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
  Expert -> [#"0", #"-1", #"-2", #"-2", #"-3", #"-4", #"-4", #"-5", #"-7", #frost, #frost, #frost, Skull, Skull, Cultist, Tablet, AutoFail, ElderSign]
{- FOURMOLU_ENABLE -}
