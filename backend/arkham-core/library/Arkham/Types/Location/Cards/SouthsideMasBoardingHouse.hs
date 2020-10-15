{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.SouthsideMasBoardingHouse
  ( SouthsideMasBoardingHouse(..)
  , southsideMasBoardingHouse
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

southsideMasBoardingHouse :: SouthsideMasBoardingHouse
southsideMasBoardingHouse = SouthsideMasBoardingHouse $ baseAttrs
  "01127"
  "Southside"
  2
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]
  [Arkham]

instance HasModifiersFor env investigator SouthsideMasBoardingHouse where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance (ActionRunner env investigator) => HasActions env investigator SouthsideMasBoardingHouse where
  getActions i NonFast (SouthsideMasBoardingHouse attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions i NonFast attrs
      unused <- getIsUnused i (ability attrs)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) (ability attrs)
           | unused
             && atLocation i attrs
             && hasActionsRemaining i Nothing locationTraits
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Ally])
    _ -> SouthsideMasBoardingHouse <$> runMessage msg attrs
