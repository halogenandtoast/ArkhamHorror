module Arkham.Investigator.Types where

import Arkham.Field
import Arkham.Prelude

data Investigator

instance Data Investigator
instance Show Investigator
instance Eq Investigator
instance Ord Investigator
instance ToJSON Investigator

data InvestigatorForm

instance Data InvestigatorForm
instance Show InvestigatorForm
instance Eq InvestigatorForm
instance Ord InvestigatorForm
instance ToJSON InvestigatorForm
instance FromJSON InvestigatorForm

instance Show (Field Investigator a)
instance Ord (Field Investigator a)
instance Typeable a => Data (Field Investigator a)
instance Typeable a => FromJSON (Field Investigator a)
instance ToJSON (Field Investigator a)

data SomeInvestigator
