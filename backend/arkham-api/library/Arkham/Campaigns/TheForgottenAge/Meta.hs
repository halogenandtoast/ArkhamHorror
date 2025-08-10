module Arkham.Campaigns.TheForgottenAge.Meta where

import Arkham.Id
import Arkham.Prelude
import Data.Map.Strict qualified as Map

data Metadata = Metadata
  { supplyPoints :: Map InvestigatorId Int
  , yithians :: Set InvestigatorId
  , expeditionLeader :: Maybe InvestigatorId
  , bonusXp :: Map InvestigatorId Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance Semigroup Metadata where
  Metadata a1 b1 c1 d1 <> Metadata a2 b2 c2 d2 = Metadata (a1 <> a2) (b1 <> b2) (c1 <|> c2) (Map.unionWith (+) d1 d2)

instance Monoid Metadata where
  mempty = Metadata mempty mempty Nothing mempty

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \o -> do
    supplyPoints <- o .: "supplyPoints"
    yithians <- o .: "yithians"
    expeditionLeader <- o .:? "expeditionLeader"
    bonusXp <- o .:? "bonusXp" .!= mempty
    pure $ Metadata {..}
