module Arkham.Helpers.Customization (module Arkham.Helpers.Customization, module Arkham.Customization) where

import Arkham.Asset.Types (Asset, AssetAttrs (..))
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Customization
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Prelude
import Arkham.Projection
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map

getHasCustomization :: HasGame m => AssetId -> Customization -> m Bool
getHasCustomization aid c = (`hasCustomization` c) <$> getAttrs @Asset aid

hasCustomization :: AssetAttrs -> Customization -> Bool
hasCustomization attrs n = case mCustomizationIndex of
  Nothing -> False
  Just i -> valueOf i == requiredXp
 where
  customizations = assetCustomizations attrs
  valueOf x = IntMap.findWithDefault 0 x customizations
  requiredXp = Map.findWithDefault 100 n cardCustomizations
  mCustomizationIndex = elemIndex n $ Map.keys cardCustomizations
  cardCustomizations = cdCustomizations $ toCardDef attrs
