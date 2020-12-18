{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Location.Cards.Northside
  ( Northside(..)
  , northside
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Northside = Northside Attrs
  deriving newtype (Show, ToJSON, FromJSON)

northside :: Northside
northside = Northside $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "01134"
    (LocationName "Northside" Nothing)
    EncounterSet.TheMidnightMasks
    3
    (PerPlayer 2)
    T
    [Diamond, Triangle]
    [Arkham]

instance HasModifiersFor env Northside where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = base { abilityLimit = GroupLimit PerGame 1 }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 5])

instance ActionRunner env => HasActions env Northside where
  getActions iid NonFast (Northside attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions iid window (Northside attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env Northside where
  runMessage msg l@(Northside attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (GainClues iid 2)
    _ -> Northside <$> runMessage msg attrs
