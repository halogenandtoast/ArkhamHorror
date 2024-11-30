module Arkham.Campaigns.EdgeOfTheEarth.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
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
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted
import Arkham.PlayerCard
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Setup
import Arkham.Treachery.Types (Field (TreacheryCardId))
import Arkham.Window (WindowType (ScenarioEvent))
import GHC.Records

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

getShelterValue :: HasCardCode a => a -> Maybe Int
getShelterValue a = do
  def <- lookupCardDef (toCardCode a)
  val <- lookup "shelter" (cdMeta def)
  maybeResult val

data Partner = Partner
  { partnerCardCode :: CardCode
  , partnerDamage :: Int
  , partnerHorror :: Int
  , partnerStatus :: PartnerStatus
  }

instance HasField "status" Partner PartnerStatus where
  getField = partnerStatus

instance HasField "damage" Partner Int where
  getField = partnerDamage

instance HasField "horror" Partner Int where
  getField = partnerHorror

instance HasField "cardCode" Partner CardCode where
  getField = partnerCardCode

instance HasCardCode Partner where
  toCardCode = (.cardCode)

getPartnersWithStatus :: HasGame m => (PartnerStatus -> Bool) -> m [Partner]
getPartnersWithStatus f = do
  partners <- view partnersL <$> getCampaignLog
  pure $ flip mapMaybe (mapToList partners) \(cardCode, partner) -> do
    guard $ f partner.status
    pure
      $ Partner
        { partnerCardCode = cardCode
        , partnerDamage = partner.damage
        , partnerHorror = partner.horror
        , partnerStatus = partner.status
        }

getRemainingPartners :: HasGame m => m [Partner]
getRemainingPartners = getPartnersWithStatus (`elem` [Safe, Resolute])

getPartner :: (HasGame m, HasCardCode a) => a -> m Partner
getPartner (toCardCode -> cardCode) = do
  partners <- view partnersL <$> getCampaignLog
  pure $ fromJustNote "Not a valid partner" $ lookup cardCode partners >>= \partner ->
    pure
      $ Partner
        { partnerCardCode = cardCode
        , partnerDamage = partner.damage
        , partnerHorror = partner.horror
        , partnerStatus = partner.status
        }

setPartnerStatus :: (ReverseQueue m, HasCardCode a) => a -> PartnerStatus -> m ()
setPartnerStatus a = push . Msg.SetPartnerStatus (toCardCode a)

partnerEliminated :: (ReverseQueue m, HasCardCode a) => a -> m ()
partnerEliminated a = setPartnerStatus a Eliminated
