module Arkham.Helpers.Customization (module Arkham.Helpers.Customization, module Arkham.Customization) where

import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Customization
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Prelude
import Arkham.Projection
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import GHC.Records

getHasCustomization
  :: forall a m
   . ( HasGame m
     , IdOf a ~ EntityId a
     , Projection a
     , HasCardDef (EntityAttrs a)
     , HasField "customizations" (EntityAttrs a) (IntMap Int)
     )
  => IdOf a
  -> Customization
  -> m Bool
getHasCustomization aid c = (`hasCustomization` c) <$> getAttrs @a aid

hasCustomization
  :: (HasCardDef a, HasField "customizations" a (IntMap Int)) => a -> Customization -> Bool
hasCustomization attrs n = case mCustomizationIndex of
  Nothing -> False
  Just i -> valueOf i == requiredXp
 where
  customizations = attrs.customizations
  valueOf x = IntMap.findWithDefault 0 x customizations
  requiredXp = Map.findWithDefault 100 n cardCustomizations
  mCustomizationIndex = elemIndex n $ Map.keys cardCustomizations
  cardCustomizations = cdCustomizations $ toCardDef attrs
