{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.AdamLynch
  ( adamLynch
  , AdamLynch(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype AdamLynch = AdamLynch Attrs
  deriving newtype (Show, ToJSON, FromJSON)

adamLynch :: AssetId -> AdamLynch
adamLynch uuid = AdamLynch $ baseAttrs uuid "02139"

instance HasActions env AdamLynch where
  getActions iid window (AdamLynch attrs) = getActions iid window attrs

instance HasId (Maybe LocationId) env LocationMatcher => HasModifiersFor env AdamLynch where
  getModifiersFor (InvestigatorSource iid) (LocationTarget lid) (AdamLynch attrs)
    = do
      isSecurityOffice <- elem lid
        <$> getId @(Maybe LocationId) (LocationWithTitle "Security Office")
      pure $ toModifiers
        attrs
        [ ActionCostModifier (-1) | isSecurityOffice && ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env AdamLynch where
  runMessage msg a@(AdamLynch attrs) = case msg of
    AssetDefeated aid | aid == assetId attrs ->
      a <$ unshiftMessages [AddToken Tablet, RemoveFromGame (AssetTarget aid)]
    _ -> AdamLynch <$> runMessage msg attrs
