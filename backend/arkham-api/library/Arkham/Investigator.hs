{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator (
  module Arkham.Investigator,
  module Arkham.Investigator.Types,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity.TH
import Arkham.Id
import Arkham.Investigator.Investigators
import Arkham.Investigator.Runner hiding (allInvestigators)
import Arkham.Investigator.Types
import Data.Aeson (Result (..))
import Data.Typeable

lookupInvestigator :: InvestigatorId -> PlayerId -> Investigator
lookupInvestigator iid pid = case lookup (toCardCode iid) allInvestigators of
  Nothing -> lookupPromoInvestigator iid pid
  Just c -> overAttrs (artL .~ toCardCode iid) $ toInvestigator c pid

normalizeInvestigatorId :: InvestigatorId -> InvestigatorId
normalizeInvestigatorId iid = findWithDefault iid iid promoInvestigators

{- | Handle promo investigators

Some investigators have book versions that are just alternative art
with some replacement cards. Since these investigators are functionally
the same, we proxy the lookup to their non-promo version.
-}
promoInvestigators :: Map InvestigatorId InvestigatorId
promoInvestigators =
  mapFromList
    [ ("98001", "02003") -- Jenny Barnes
    , ("98004", "01001") -- Roland Banks
    , ("98007", "08004") -- Norman Withers
    , ("98010", "05001") -- Carolyn Fern
    , ("98013", "07005") -- Silas Marsh
    , ("98016", "07004") -- Dexter Drake
    , ("99001", "05006") -- Marie Lambeau
    ]

lookupPromoInvestigator :: InvestigatorId -> PlayerId -> Investigator
lookupPromoInvestigator iid pid = case lookup iid promoInvestigators of
  Nothing -> error $ "Unknown promo investigator: " <> show iid
  Just iid' -> overAttrs (artL .~ toCardCode iid) $ lookupInvestigator iid' pid

instance FromJSON Investigator where
  parseJSON = withObject "Investigator" $ \o -> do
    cCode <- o .: "cardCode"
    withInvestigatorCardCode cCode
      $ \(SomeInvestigator @a) -> Investigator <$> parseJSON @a (Object o)

withInvestigatorCardCode :: CardCode -> (SomeInvestigator -> r) -> r
withInvestigatorCardCode cCode f = case lookup cCode allInvestigators of
  Nothing -> case cCode of
    "04244" -> f (SomeInvestigator @BodyOfAYithian)
    "05046" -> f (SomeInvestigator @GavriellaMizrah)
    "05047" -> f (SomeInvestigator @JeromeDavids)
    "05048" -> f (SomeInvestigator @ValentinoRivas)
    "05049" -> f (SomeInvestigator @PennyWhite)
    _ -> error ("invalid investigators: " <> show cCode)
  Just (SomeInvestigatorCard (_ :: InvestigatorCard a)) -> f (SomeInvestigator @a)

allInvestigators :: Map CardCode SomeInvestigatorCard
allInvestigators =
  mapFromList
    $ concatMap
      (\c -> (,c) <$> someInvestigatorCardCodes c)
      [ SomeInvestigatorCard rolandBanks
      , SomeInvestigatorCard daisyWalker
      , SomeInvestigatorCard skidsOToole
      , SomeInvestigatorCard agnesBaker
      , SomeInvestigatorCard wendyAdams
      , SomeInvestigatorCard zoeySamaras
      , SomeInvestigatorCard rexMurphy
      , SomeInvestigatorCard jennyBarnes
      , SomeInvestigatorCard jimCulver
      , SomeInvestigatorCard ashcanPete
      , SomeInvestigatorCard markHarrigan
      , SomeInvestigatorCard minhThiPhan
      , SomeInvestigatorCard sefinaRousseau
      , SomeInvestigatorCard akachiOnyele
      , SomeInvestigatorCard williamYorick
      , SomeInvestigatorCard lolaHayes
      , SomeInvestigatorCard leoAnderson
      , SomeInvestigatorCard ursulaDowns
      , SomeInvestigatorCard finnEdwards
      , SomeInvestigatorCard fatherMateo
      , SomeInvestigatorCard calvinWright
      , SomeInvestigatorCard carolynFern
      , SomeInvestigatorCard joeDiamond
      , SomeInvestigatorCard prestonFairmont
      , SomeInvestigatorCard dianaStanley
      , SomeInvestigatorCard ritaYoung
      , SomeInvestigatorCard marieLambeau
      , SomeInvestigatorCard gavriellaMizrah
      , SomeInvestigatorCard jeromeDavids
      , SomeInvestigatorCard valentinoRivas
      , SomeInvestigatorCard pennyWhite
      , SomeInvestigatorCard tommyMuldoon
      , SomeInvestigatorCard mandyThompson
      , SomeInvestigatorCard tonyMorgan
      , SomeInvestigatorCard lukeRobinson
      , SomeInvestigatorCard patriceHathaway
      , SomeInvestigatorCard sisterMary
      , SomeInvestigatorCard amandaSharpe
      , SomeInvestigatorCard trishScarborough
      , SomeInvestigatorCard dexterDrake
      , SomeInvestigatorCard silasMarsh
      , SomeInvestigatorCard danielaReyes
      , SomeInvestigatorCard normanWithers
      , SomeInvestigatorCard montereyJack
      , SomeInvestigatorCard lilyChen
      , SomeInvestigatorCard bobJenkins
      , SomeInvestigatorCard carsonSinclair
      , SomeInvestigatorCard vincentLee
      , SomeInvestigatorCard kymaniJones
      , SomeInvestigatorCard aminaZidane
      , SomeInvestigatorCard darrellSimmons
      , SomeInvestigatorCard charlieKane
      , SomeInvestigatorCard wilsonRichards
      , SomeInvestigatorCard kateWinthrop
      , SomeInvestigatorCard alessandraZorzi
      , SomeInvestigatorCard kohakuNarukami
      , SomeInvestigatorCard hankSamson
      , SomeInvestigatorCard marionTavares
      , SomeInvestigatorCard luciusGalloway
      , SomeInvestigatorCard agathaCrane_Seeker
      , SomeInvestigatorCard agathaCrane_Mystic
      , SomeInvestigatorCard michaelMcGlen
      , SomeInvestigatorCard gloriaGoldberg
      , SomeInvestigatorCard georgeBarnaby
      , SomeInvestigatorCard lostHomunculus
      , SomeInvestigatorCard nathanielCho
      , SomeInvestigatorCard harveyWalters
      , SomeInvestigatorCard winifredHabbamock
      , SomeInvestigatorCard stellaClark
      , SomeInvestigatorCard jacquelineFine
      , SomeInvestigatorCard subject5U21
      , SomeInvestigatorCard daisyWalkerParallel
      , SomeInvestigatorCard skidsOTooleParallel
      , SomeInvestigatorCard agnesBakerParallel
      , SomeInvestigatorCard rolandBanksParallel
      , SomeInvestigatorCard wendyAdamsParallel
      , SomeInvestigatorCard ashcanPeteParallel
      , SomeInvestigatorCard jimCulverParallel
      , SomeInvestigatorCard zoeySamarasParallel
      , SomeInvestigatorCard montereyJackParallel
      , SomeInvestigatorCard rexMurphyParallel
      , SomeInvestigatorCard fatherMateoParallel
      , SomeInvestigatorCard jennyBarnesParallel
      ]

becomeYithian :: Investigator -> Investigator
becomeYithian (Investigator a) =
  Investigator
    $ BodyOfAYithian
    . (`with` YithianMetadata (toJSON a))
    $ (toAttrs a)
      { investigatorHealth = 7
      , investigatorSanity = 7
      , investigatorWillpower = 2
      , investigatorIntellect = 2
      , investigatorCombat = 2
      , investigatorAgility = 2
      , investigatorCardCode = "04244"
      , investigatorClass = Neutral
      , investigatorTraits = setFromList [Monster, Yithian]
      , investigatorForm = YithianForm
      , investigatorDiscarding = Nothing
      }

becomeHomunculus :: Investigator -> Investigator
becomeHomunculus (Investigator a) =
  Investigator
    $ LostHomunculus
    $ (toAttrs a)
      { investigatorHealth = 6
      , investigatorSanity = 6
      , investigatorWillpower = 2
      , investigatorIntellect = 2
      , investigatorCombat = 2
      , investigatorAgility = 2
      , investigatorCardCode = "11068b"
      , investigatorArt = "11068b"
      , investigatorClass = Mystic
      , investigatorTraits = setFromList [Construct, Scion]
      , investigatorDiscarding = Nothing
      , investigatorForm = HomunculusForm
      }

handleInvestigator :: IsInvestigator a => Investigator -> (a -> Investigator) -> Investigator
handleInvestigator o@(Investigator a) f = maybe o f (cast a)

returnToBody :: Investigator -> Investigator
returnToBody i =
  i
    `handleInvestigator` ( \(BodyOfAYithian (attrs `With` meta)) -> case fromJSON (originalBody meta) of
                             Success x ->
                               overAttrs (\a -> a {investigatorSettings = investigatorSettings attrs <> investigatorSettings a}) x
                             _ -> error "Investigator mind is too corrupted to return to their body"
                         )
