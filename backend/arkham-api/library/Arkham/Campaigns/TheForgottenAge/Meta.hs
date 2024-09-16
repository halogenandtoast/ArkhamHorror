module Arkham.Campaigns.TheForgottenAge.Meta where

import Arkham.Id
import Arkham.Prelude

data Metadata = Metadata
  { supplyPoints :: Map InvestigatorId Int
  , yithians :: Set InvestigatorId
  , expeditionLeader :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Metadata where
  Metadata a1 b1 c1 <> Metadata a2 b2 c2 = Metadata (a1 <> a2) (b1 <> b2) (c1 <|> c2)

instance Monoid Metadata where
  mempty = Metadata mempty mempty Nothing
