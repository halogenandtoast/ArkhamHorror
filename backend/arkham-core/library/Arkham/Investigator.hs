{-# LANGUAGE TemplateHaskell #-}
module Arkham.Investigator
  ( module Arkham.Investigator
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Investigators
import Arkham.Investigator.Runner
import Arkham.Message
import Arkham.Modifier
import Data.Aeson.TH

$(buildEntity "Investigator")
$(deriveJSON defaultOptions ''Investigator)

instance HasModifiersFor Investigator where
  getModifiersFor = $(entityF2 "Investigator" "getModifiersFor")

instance HasTokenValue Investigator where
  getTokenValue = $(entityF2 "Investigator" "getTokenValue")

instance HasAbilities Investigator where
  getAbilities = $(entityF "Investigator" "getAbilities")

instance RunMessage Investigator where
  runMessage msg i = do
    modifiers' <- getModifiers (toSource i) (toTarget i)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    $(entityRunMessage "Investigator") msg' i

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = mapFromList $ map
  (InvestigatorId . cbCardCode &&& ($ ()) . cbCardBuilder)
  $(buildEntityLookupList "Investigator")

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid =
  fromMaybe (lookupPromoInvestigator iid) $ lookup iid allInvestigators

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

instance Entity Investigator where
  type EntityId Investigator = InvestigatorId
  type EntityAttrs Investigator = InvestigatorAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Investigator" "toAttrs")

instance TargetEntity Investigator where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Investigator where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance ToGameLoggerFormat Investigator where
  format = format . toAttrs
