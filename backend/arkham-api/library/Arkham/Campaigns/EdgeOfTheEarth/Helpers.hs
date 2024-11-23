module Arkham.Campaigns.EdgeOfTheEarth.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Deck (toDeck)
import Arkham.EncounterSet (EncounterSet (Tekelili))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Campaign
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (..))
import Arkham.Message.Lifted
import Arkham.PlayerCard
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Setup
import Arkham.Treachery.Types (Field (TreacheryCardId))
import Arkham.Window (WindowType (ScenarioEvent))

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "edgeOfTheEarth" a

expeditionTeam :: NonEmpty CardDef
expeditionTeam =
  Assets.drAmyKenslerProfessorOfBiology
    :| [ Assets.professorWilliamDyerProfessorOfGeology
       , Assets.danforthBrilliantStudent
       , Assets.roaldEllsworthIntrepidExplorer
       , Assets.takadaHirokoAeroplaneMechanic
       , Assets.averyClaypoolAntarcticGuide
       , Assets.drMalaSinhaDaringPhysician
       , Assets.jamesCookieFredericksDubiousChoice
       , Assets.eliyahAshevakDogHandler
       ]

shelterValue :: (HasGame m, AsId location, IdOf location ~ LocationId) => location -> m (Maybe Int)
shelterValue location = do
  card <- field LocationCard (asId location)
  pure $ lookup "shelter" (toCardDef card).meta >>= maybeResult

whenRecoveredSupply :: HasGame m => Supply -> m () -> m ()
whenRecoveredSupply supply action = whenM (hasSupply supply) action

hasSupply :: HasGame m => Supply -> m Bool
hasSupply supply = inRecordSet (toJSON supply) SuppliesRecovered

recoverSupply :: ReverseQueue m => Supply -> m ()
recoverSupply supply = recordSetInsert SuppliesRecovered [toJSON supply]

getTekelili :: HasGame m => Int -> m [Card]
getTekelili n = take n <$> getScenarioDeck TekeliliDeck

addTekeliliDeck :: ReverseQueue m => ScenarioBuilderT m ()
addTekeliliDeck = addExtraDeck TekeliliDeck =<< shuffle =<< gatherTekelili

gatherTekelili :: (HasGame m, CardGen m) => m [Card]
gatherTekelili = do
  storyCards <- concat . toList <$> getCampaignStoryCards
  let storyCardDefs = map toCardDef storyCards
  let filteredDefs = foldl' (flip (deleteFirstMatch . (==))) defs storyCardDefs
  concat <$> for filteredDefs \def ->
    traverse genCard $ replicate (fromMaybe 0 (cdEncounterSetQuantity def)) def
 where
  defs =
    filter ((== Just Tekelili) . cdEncounterSet) $ toList allPlayerCards

addTekelili :: ReverseQueue m => InvestigatorId -> [Card] -> m ()
addTekelili iid cards = batched \batchId -> do
  checkWhen $ ScenarioEvent "shuffleTekelili" (toJSON (batchId, cards))
  traverse_ (addCampaignCardToDeck iid) cards

resolveTekelili
  :: (ReverseQueue m, AsId tekelili, IdOf tekelili ~ TreacheryId) => InvestigatorId -> tekelili -> m ()
resolveTekelili iid tekelili = do
  cardId <- field TreacheryCardId (asId tekelili)
  mods <- getModifiers cardId
  let deck = if PlaceOnBottomOfDeckInsteadOfDiscard `elem` mods then toDeck iid else toDeck TekeliliDeck
  putOnBottomOfDeck iid deck (asId tekelili)
