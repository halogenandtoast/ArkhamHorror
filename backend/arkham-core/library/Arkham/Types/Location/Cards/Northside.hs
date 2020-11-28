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
northside = Northside $ (baseAttrs
                          "01134"
                          "Northside"
                          EncounterSet.TheMidnightMasks
                          3
                          (PerPlayer 2)
                          T
                          [Diamond, Triangle]
                          [Arkham]
                        )
  { locationVictory = Just 1
  }

instance HasModifiersFor env Northside where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env Northside where
  getActions iid NonFast (Northside attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions iid NonFast attrs
    unused <- getGroupIsUnused (ability attrs)
    resourceCount <- getResourceCount iid
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList locationTraits)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction iid (ability attrs)
         | resourceCount
           >= 5
           && unused
           && iid
           `member` locationInvestigators
           && hasActionsRemaining
         ] -- GROUP LIMIT
  getActions iid window (Northside attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env Northside where
  runMessage msg l@(Northside attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages [SpendResources iid 5, GainClues iid 2]
    _ -> Northside <$> runMessage msg attrs
