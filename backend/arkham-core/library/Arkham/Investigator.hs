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
import Data.Typeable

data Investigator = forall a. IsInvestigator a => Investigator a

instance Eq Investigator where
  (Investigator (a :: a)) == (Investigator (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Investigator where
  show (Investigator a) = show a

instance ToJSON Investigator where
  toJSON (Investigator a) = toJSON a

instance HasModifiersFor Investigator where
  getModifiersFor source target (Investigator a) = getModifiersFor source target a

instance HasTokenValue Investigator where
  getTokenValue iid tokenFace (Investigator a) = getTokenValue iid tokenFace a

instance HasAbilities Investigator where
  getAbilities (Investigator a) = getAbilities a

instance RunMessage Investigator where
  runMessage msg i@(Investigator a) = do
    modifiers' <- getModifiers (toSource i) (toTarget i)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Investigator <$> runMessage msg' a

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
  toAttrs (Investigator a) = toAttrs a
  overAttrs f (Investigator a) = Investigator $ overAttrs f a

instance TargetEntity Investigator where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Investigator where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance ToGameLoggerFormat Investigator where
  format = format . toAttrs

instance FromJSON Investigator where
  parseJSON v = flip (withObject "Investigator") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case cCode of
      "01001" -> Investigator . RolandBanks <$> parseJSON v
      "01002" -> Investigator . DaisyWalker <$> parseJSON v
      "01003" -> Investigator . SkidsOToole <$> parseJSON v
      "01004" -> Investigator . AgnesBaker <$> parseJSON v
      "01005" -> Investigator . WendyAdams <$> parseJSON v
      "02001" -> Investigator . ZoeySamaras <$> parseJSON v
      "02002" -> Investigator . RexMurphy <$> parseJSON v
      "02003" -> Investigator . JennyBarnes <$> parseJSON v
      "02004" -> Investigator . JimCulver <$> parseJSON v
      "02005" -> Investigator . AshcanPete <$> parseJSON v
      "03001" -> Investigator . MarkHarrigan <$> parseJSON v
      "03002" -> Investigator . MinhThiPhan <$> parseJSON v
      "03003" -> Investigator . SefinaRousseau <$> parseJSON v
      "03004" -> Investigator . AkachiOnyele <$> parseJSON v
      "03005" -> Investigator . WilliamYorick <$> parseJSON v
      "03006" -> Investigator . LolaHayes <$> parseJSON v
      "04001" -> Investigator . LeoAnderson <$> parseJSON v
      "04002" -> Investigator . UrsulaDowns <$> parseJSON v
      "04003" -> Investigator . FinnEdwards <$> parseJSON v
      "08004" -> Investigator . NormanWithers <$> parseJSON v
      "60101" -> Investigator . NathanielCho <$> parseJSON v
      "60501" -> Investigator . StellaClark <$> parseJSON v
      "90001" -> Investigator . DaisyWalkerParallel <$> parseJSON v
      _ -> error "invalid investigator"

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = mapFromList $ map
  (InvestigatorId . cbCardCode &&& ($ ()) . cbCardBuilder)
  [ Investigator <$> rolandBanks
  , Investigator <$> daisyWalker
  , Investigator <$> skidsOToole
  , Investigator <$> agnesBaker
  , Investigator <$> wendyAdams
  , Investigator <$> zoeySamaras
  , Investigator <$> rexMurphy
  , Investigator <$> jennyBarnes
  , Investigator <$> jimCulver
  , Investigator <$> ashcanPete
  , Investigator <$> markHarrigan
  , Investigator <$> minhThiPhan
  , Investigator <$> sefinaRousseau
  , Investigator <$> akachiOnyele
  , Investigator <$> williamYorick
  , Investigator <$> lolaHayes
  , Investigator <$> leoAnderson
  , Investigator <$> ursulaDowns
  , Investigator <$> finnEdwards
  , Investigator <$> normanWithers
  , Investigator <$> nathanielCho
  , Investigator <$> stellaClark
  , Investigator <$> daisyWalkerParallel
  ]
