module Arkham.Helpers.Customization (module Arkham.Helpers.Customization, module Arkham.Customization) where

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Customization
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Data.List (elemIndex)
import GHC.Records

data CustomizationChoiceType
  = CustomizationCardChoice CardMatcher
  | CustomizationSkillChoice
  | CustomizationTraitChoice
  | CustomizationIndexChoice [Text]
  deriving stock (Show, Eq)

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
cardRemainingCheckMarks :: Card -> Customization -> Maybe Int
cardRemainingCheckMarks card c = case card of
  PlayerCard pc -> remainingCheckMarks_ cardCustomizations (pcCustomizations pc) c
  _ -> Nothing
 where
  cardCustomizations = cdCustomizations $ toCardDef card
customizationIndex :: HasCardDef a => a -> Customization -> Maybe Int
customizationIndex a c = elemIndex c $ keys $ cdCustomizations (toCardDef a)
choicesRequired :: Customization -> [CustomizationChoiceType]
choicesRequired = \case
  Versatile -> [CustomizationTraitChoice]
  Specialist -> [CustomizationTraitChoice]
  Specialist2 -> [CustomizationTraitChoice]
  EldritchInk -> [CustomizationSkillChoice]
  EldritchInk2 -> [CustomizationSkillChoice]
  EndlessInkwell ->
    [ CustomizationCardChoice $ #asset <> oneOf [#tome, #spell]
    , CustomizationCardChoice $ #asset <> oneOf [#tome, #spell]
    ]
  Dominance -> [CustomizationIndexChoice ["arcane", "ally"]]
  _ -> []
