module Arkham.Campaigns.EdgeOfTheEarth.Partner where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Prelude
import GHC.Records

-- ** Partner Types ** --

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

-- ** Partner Helpers ** --

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

getPartnersWithStatus :: HasGame m => (PartnerStatus -> Bool) -> m [Partner]
getPartnersWithStatus f = do
  partners <- view partnersL <$> getCampaignLog
  pure $ flip mapMaybe (mapToList partners) \(cardCode, partner) -> do
    guard $ f partner.status
    pure
      $ Partner
        { partnerCardCode = if partner.status == Resolute then toResolute cardCode else cardCode
        , partnerDamage = partner.damage
        , partnerHorror = partner.horror
        , partnerStatus = partner.status
        }

toResolute :: CardCode -> CardCode
toResolute = \case
  c
    | c == Assets.professorWilliamDyerProfessorOfGeology.cardCode ->
        Assets.professorWilliamDyerProfessorOfGeologyResolute.cardCode
  c
    | c == Assets.danforthBrilliantStudent.cardCode ->
        Assets.danforthBrilliantStudentResolute.cardCode
  c
    | c == Assets.eliyahAshevakDogHandler.cardCode ->
        Assets.eliyahAshevakDogHandlerResolute.cardCode
  c
    | c == Assets.drMalaSinhaDaringPhysician.cardCode ->
        Assets.drMalaSinhaDaringPhysicianResolute.cardCode
  c
    | c == Assets.averyClaypoolAntarcticGuide.cardCode ->
        Assets.averyClaypoolAntarcticGuideResolute.cardCode
  c
    | c == Assets.jamesCookieFredericksDubiousChoice.cardCode ->
        Assets.jamesCookieFredericksDubiousChoiceResolute.cardCode
  c
    | c == Assets.drAmyKenslerProfessorOfBiology.cardCode ->
        Assets.drAmyKenslerProfessorOfBiologyResolute.cardCode
  c
    | c == Assets.roaldEllsworthIntrepidExplorer.cardCode ->
        Assets.roaldEllsworthIntrepidExplorerResolute.cardCode
  c
    | c == Assets.takadaHirokoAeroplaneMechanic.cardCode ->
        Assets.takadaHirokoAeroplaneMechanicResolute.cardCode
  _ -> error "can not make resolute"

getRemainingPartners :: HasGame m => m [Partner]
getRemainingPartners = getPartnersWithStatus (`elem` [Safe, Resolute])

getPartner :: (HasGame m, HasCardCode a) => a -> m Partner
getPartner (toCardCode -> cardCode) = do
  partners <- view partnersL <$> getCampaignLog
  pure $ fromJustNote "Not a valid partner" $ lookup cardCode partners >>= \partner ->
    pure
      $ Partner
        { partnerCardCode = if partner.status == Resolute then toResolute cardCode else cardCode
        , partnerDamage = partner.damage
        , partnerHorror = partner.horror
        , partnerStatus = partner.status
        }

getPartnerIsAlive :: (HasGame m, HasCardCode a) => a -> m Bool
getPartnerIsAlive x = (`elem` [Safe, Resolute]) <$> getPartnerStatus x

getPartnerStatus :: (HasGame m, HasCardCode a) => a -> m PartnerStatus
getPartnerStatus (toPartnerCode -> cardCode) = do
  partners <- view partnersL <$> getCampaignLog
  pure $ fromJustNote "Not a valid partner" $ lookup cardCode partners <&> \partner -> partner.status

toPartnerCode :: (HasCallStack, HasCardCode a) => a -> CardCode
toPartnerCode a = case toCardCode a of
  c | c `elem` expeditionTeamCodes -> c
  c
    | c == Enemies.professorWilliamDyerProfessorOfGeology.cardCode ->
        Assets.professorWilliamDyerProfessorOfGeology.cardCode
  c | c == Enemies.danforthBrilliantStudent.cardCode -> Assets.danforthBrilliantStudent.cardCode
  c
    | c == Enemies.roaldEllsworthIntrepidExplorer.cardCode ->
        Assets.roaldEllsworthIntrepidExplorer.cardCode
  c
    | c == Enemies.takadaHirokoAeroplaneMechanic.cardCode ->
        Assets.takadaHirokoAeroplaneMechanic.cardCode
  c | c == Enemies.averyClaypoolAntarcticGuide.cardCode -> Assets.averyClaypoolAntarcticGuide.cardCode
  c | c == Enemies.drMalaSinhaDaringPhysician.cardCode -> Assets.drMalaSinhaDaringPhysician.cardCode
  c
    | c == Enemies.jamesCookieFredericksDubiousChoice.cardCode ->
        Assets.jamesCookieFredericksDubiousChoice.cardCode
  c | c == Enemies.eliyahAshevakDogHandler.cardCode -> Assets.eliyahAshevakDogHandler.cardCode
  c
    | c == Assets.professorWilliamDyerProfessorOfGeologyResolute.cardCode ->
        Assets.professorWilliamDyerProfessorOfGeology.cardCode
  c
    | c == Assets.danforthBrilliantStudentResolute.cardCode ->
        Assets.danforthBrilliantStudent.cardCode
  c
    | c == Assets.eliyahAshevakDogHandlerResolute.cardCode ->
        Assets.eliyahAshevakDogHandler.cardCode
  c
    | c == Assets.drMalaSinhaDaringPhysicianResolute.cardCode ->
        Assets.drMalaSinhaDaringPhysician.cardCode
  c
    | c == Assets.averyClaypoolAntarcticGuideResolute.cardCode ->
        Assets.averyClaypoolAntarcticGuide.cardCode
  c
    | c == Assets.jamesCookieFredericksDubiousChoiceResolute.cardCode ->
        Assets.jamesCookieFredericksDubiousChoice.cardCode
  c
    | c == Assets.drAmyKenslerProfessorOfBiologyResolute.cardCode ->
        Assets.drAmyKenslerProfessorOfBiology.cardCode
  c
    | c == Assets.roaldEllsworthIntrepidExplorerResolute.cardCode ->
        Assets.roaldEllsworthIntrepidExplorer.cardCode
  c
    | c == Assets.takadaHirokoAeroplaneMechanicResolute.cardCode ->
        Assets.takadaHirokoAeroplaneMechanic.cardCode
  _ -> error "Unknown partner"
 where
  expeditionTeamCodes = map toCardCode (toList expeditionTeam)
