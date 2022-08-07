{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Investigator
  ( module Arkham.Investigator
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity.TH
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Investigators
import Arkham.Investigator.Runner
import Arkham.Message

instance RunMessage Investigator where
  runMessage msg i@(Investigator a) = do
    modifiers' <- getModifiers (toTarget i)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Investigator <$> runMessage msg' a

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid = case lookup (unInvestigatorId iid) allInvestigators of
  Nothing -> lookupPromoInvestigator iid
  Just c -> toInvestigator c

-- | Handle promo investigators
--
-- Some investigators have book versions that are just alternative art
-- with some replacement cards. Since these investigators are functionally
-- the same, we proxy the lookup to their non-promo version.
--
-- Parallel investigators will need to be handled differently since they
-- are not functionally the same.
--
lookupPromoInvestigator :: InvestigatorId -> Investigator
lookupPromoInvestigator "98001" = lookupInvestigator "02003" -- Jenny Barnes
lookupPromoInvestigator "98004" = lookupInvestigator "01001" -- Roland Banks
lookupPromoInvestigator iid     = error $ "Unknown investigator: " <> show iid

instance FromJSON Investigator where
  parseJSON v = flip (withObject "Investigator") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withInvestigatorCardCode cCode $ \(_ :: InvestigatorCard a) -> Investigator <$> parseJSON @a v

withInvestigatorCardCode
  :: CardCode
  -> (forall a. IsInvestigator a => InvestigatorCard a -> r)
  -> r
withInvestigatorCardCode cCode f =
  case lookup cCode allInvestigators of
    Nothing -> error "invalid investigators"
    Just (SomeInvestigatorCard a) -> f a

allInvestigators :: HashMap CardCode SomeInvestigatorCard
allInvestigators = mapFromList $ map
  (toFst someInvestigatorCardCode)
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
  , SomeInvestigatorCard normanWithers
  , SomeInvestigatorCard nathanielCho
  , SomeInvestigatorCard stellaClark
  , SomeInvestigatorCard daisyWalkerParallel
  ]
