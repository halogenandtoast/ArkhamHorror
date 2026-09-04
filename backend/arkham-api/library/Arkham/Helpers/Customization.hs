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
import Arkham.SlotType
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

{- | Slots the card will occupy once in play, adjusted for customizations that move or
remove a slot. The in-play asset applies the same change through its own HasModifiersFor,
but playability is checked while the card is still in hand, where no asset entity exists
to supply modifiers.
-}
customizedSlots :: Card -> [SlotType]
customizedSlots card =
  foldl' apply (cdSlots def) (getCustomizations_ (cdCustomizations def) customizations)
 where
  def = toCardDef card
  customizations = case card of
    PlayerCard pc -> pcCustomizations pc
    _ -> mempty
  indexes = [i | ChosenIndex i <- concatMap snd (toList customizations)]
  insteadOf added removed slots = added : filter (/= removed) slots
  apply slots = \case
    Enchanted -> insteadOf #arcane #body slots -- Hunter's Armor
    ImbuedInk -> insteadOf #arcane #body slots -- Living Ink
    Dominance -> filter (/= if 0 `elem` indexes then #arcane else #ally) slots -- Summoned Servitor
    _ -> slots

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
