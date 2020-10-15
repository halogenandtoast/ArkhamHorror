{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Northside
  ( Northside(..)
  , northside
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Northside = Northside Attrs
  deriving newtype (Show, ToJSON, FromJSON)

northside :: Northside
northside =
  Northside
    $ (baseAttrs
        "01134"
        "Northside"
        3
        (PerPlayer 2)
        T
        [Diamond, Triangle]
        [Arkham]
      )
        { locationVictory = Just 1
        }

instance HasModifiersFor env investigator Northside where
  getModifiersFor _ _ _ = pure []


ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance (ActionRunner env investigator) => HasActions env investigator Northside where
  getActions i NonFast (Northside attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i NonFast attrs
    unused <- getGroupIsUnused (ability attrs)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction (getId () i) (ability attrs)
         | resourceCount i
           >= 5
           && unused
           && atLocation i attrs
           && hasActionsRemaining i Nothing locationTraits
         ] -- GROUP LIMIT
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env Northside where
  runMessage msg l@(Northside attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages [SpendResources iid 5, GainClues iid 2]
    _ -> Northside <$> runMessage msg attrs
