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

guardCustomization
  :: (Alternative f, HasCardDef a, HasField "customizations" a Customizations)
  => a
  -> Customization
  -> f b
  -> f b
guardCustomization a c b = guard (a `hasCustomization` c) *> b

getHasCustomization
  :: forall a m
   . ( HasGame m
     , IdOf a ~ EntityId a
     , Projection a
     , HasCardDef (EntityAttrs a)
     , HasField "customizations" (EntityAttrs a) Customizations
     )
  => IdOf a
  -> Customization
  -> m Bool
getHasCustomization aid c = (`hasCustomization` c) <$> getAttrs @a aid

hasCustomization
  :: (HasCardDef a, HasField "customizations" a Customizations)
  => a
  -> Customization
  -> Bool
hasCustomization attrs = hasCustomization_ cardCustomizations attrs.customizations
 where
  cardCustomizations = cdCustomizations $ toCardDef attrs

hasCustomization_ :: Map Customization Int -> Customizations -> Customization -> Bool
hasCustomization_ cardCustomizations customizations n = case mCustomizationIndex of
  Nothing -> False
  Just i -> valueOf i == requiredXp
 where
  valueOf x = fst $ IntMap.findWithDefault (0, []) x customizations
  requiredXp = Map.findWithDefault 100 n cardCustomizations
  mCustomizationIndex = elemIndex n $ Map.keys cardCustomizations

getCustomizations
  :: (HasCardDef a, HasField "customizations" a Customizations) => a -> [Customization]
getCustomizations attrs = getCustomizations_ cardCustomizations attrs.customizations
 where
  cardCustomizations = cdCustomizations $ toCardDef attrs

getCustomizations_ :: Map Customization Int -> Customizations -> [Customization]
getCustomizations_ cardCustomizations customizations = flip concatMap (withIndex ks) \(n, k) ->
  guard (valueOf n == requiredXp k) $> k
 where
  valueOf n = fst $ IntMap.findWithDefault (0, []) n customizations
  requiredXp k = Map.findWithDefault 100 k cardCustomizations
  ks = Map.keys cardCustomizations
