module Arkham.Campaigns.EdgeOfTheEarth.Partner where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.IceAndDeath qualified as Enemies
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Prelude
import Data.Function (on)
import GHC.Records

-- ** Partner Types ** --

data Partner = Partner
  { partnerCardCode :: CardCode
  , partnerDamage :: Int
  , partnerHorror :: Int
  , partnerStatus :: PartnerStatus
  }
  deriving stock Show

instance Eq Partner where
  (==) = (==) `on` partnerCardCode

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

{- | Every partner's three faces: the ally asset the campaign log is keyed by, its
Resolute upgrade, and its possessed enemy from /Lost in the Night/. 'expeditionTeam',
'toResolute' and 'toPartnerCodeMay' all derive from this one table, so a partner cannot
be known through one face and unknown through another -- Dr. Amy Kensler's enemy face
was missing from the hand-written map, so her Forced "when defeated" ability crashed
(#5966).
-}
partnerFaces :: NonEmpty (CardDef, CardDef, CardDef)
partnerFaces =
  ( Assets.drAmyKenslerProfessorOfBiology
  , Assets.drAmyKenslerProfessorOfBiologyResolute
  , Enemies.drAmyKenslerProfessorOfBiology
  )
    :| [
         ( Assets.professorWilliamDyerProfessorOfGeology
         , Assets.professorWilliamDyerProfessorOfGeologyResolute
         , Enemies.professorWilliamDyerProfessorOfGeology
         )
       ,
         ( Assets.danforthBrilliantStudent
         , Assets.danforthBrilliantStudentResolute
         , Enemies.danforthBrilliantStudent
         )
       ,
         ( Assets.roaldEllsworthIntrepidExplorer
         , Assets.roaldEllsworthIntrepidExplorerResolute
         , Enemies.roaldEllsworthIntrepidExplorer
         )
       ,
         ( Assets.takadaHirokoAeroplaneMechanic
         , Assets.takadaHirokoAeroplaneMechanicResolute
         , Enemies.takadaHirokoAeroplaneMechanic
         )
       ,
         ( Assets.averyClaypoolAntarcticGuide
         , Assets.averyClaypoolAntarcticGuideResolute
         , Enemies.averyClaypoolAntarcticGuide
         )
       ,
         ( Assets.drMalaSinhaDaringPhysician
         , Assets.drMalaSinhaDaringPhysicianResolute
         , Enemies.drMalaSinhaDaringPhysician
         )
       ,
         ( Assets.jamesCookieFredericksDubiousChoice
         , Assets.jamesCookieFredericksDubiousChoiceResolute
         , Enemies.jamesCookieFredericksDubiousChoice
         )
       ,
         ( Assets.eliyahAshevakDogHandler
         , Assets.eliyahAshevakDogHandlerResolute
         , Enemies.eliyahAshevakDogHandler
         )
       ]

expeditionTeam :: NonEmpty CardDef
expeditionTeam = fmap (\(asset, _, _) -> asset) partnerFaces

instance HasCardDef Partner where
  toCardDef = fromMaybe (error "missing") . lookupCardDef

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
toResolute c =
  fromMaybe c
    $ listToMaybe
      [resolute.cardCode | (asset, resolute, _) <- toList partnerFaces, c == asset.cardCode]

getRemainingPartners :: HasGame m => m [Partner]
getRemainingPartners = getPartnersWithStatus (`elem` [Safe, Resolute])

getPartner :: (HasGame m, HasCardCode a) => a -> m Partner
getPartner (toCardCode -> cardCode) = do
  partners <- view partnersL <$> getCampaignLog
  pure $ fromJustNote "Not a valid partner" do
    partner <- lookup (toPartnerCode cardCode) partners <|> lookup (toResolute cardCode) partners
    pure
      $ Partner
        { partnerCardCode = if partner.status == Resolute then toResolute cardCode else cardCode
        , partnerDamage = partner.damage
        , partnerHorror = partner.horror
        , partnerStatus = partner.status
        }

getPartnerIsAlive :: (HasGame m, HasCardCode a) => a -> m Bool
getPartnerIsAlive x = (`elem` [Safe, Resolute]) <$> getPartnerStatus x

getPartnerStatus :: (HasCallStack, HasGame m, HasCardCode a) => a -> m PartnerStatus
getPartnerStatus (toPartnerCode -> cardCode) = do
  partners <- view partnersL <$> getCampaignLog
  pure $ fromJustNote ("Not a valid partner: " <> show cardCode) $ (lookup cardCode partners <|> lookup (toResolute cardCode) partners) <&> \partner -> partner.status

toPartnerCode :: (HasCallStack, HasCardCode a) => a -> CardCode
toPartnerCode a = fromMaybe (error "Unknown partner") (toPartnerCodeMay a)

toPartnerCodeMay :: HasCardCode a => a -> Maybe CardCode
toPartnerCodeMay a =
  listToMaybe
    [ asset.cardCode
    | (asset, resolute, enemy) <- toList partnerFaces
    , toCardCode a `elem` [asset.cardCode, resolute.cardCode, enemy.cardCode]
    ]
