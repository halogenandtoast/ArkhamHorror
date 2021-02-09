module Arkham.Types.ModifierData
  ( module Arkham.Types.ModifierData
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Query

newtype ModifierData = ModifierData { mdModifiers :: [Modifier] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ModifierData where
  toJSON = genericToJSON $ aesonOptions $ Just "md"
  toEncoding = genericToEncoding $ aesonOptions $ Just "md"

withModifiers
  :: ( MonadReader env m
     , TargetEntity a
     , HasModifiersFor env ()
     , HasId ActiveInvestigatorId env ()
     )
  => a
  -> m (With a ModifierData)
withModifiers a = do
  source <- InvestigatorSource . unActiveInvestigatorId <$> getId ()
  modifiers' <- getModifiersFor source (toTarget a) ()
  pure $ a `with` ModifierData modifiers'
