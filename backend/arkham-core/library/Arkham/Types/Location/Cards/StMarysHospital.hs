{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.StMarysHospital
  ( StMarysHospital(..)
  , stMarysHospital
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype StMarysHospital = StMarysHospital Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stMarysHospital :: StMarysHospital
stMarysHospital = StMarysHospital $ baseAttrs
  "01128"
  "St. Mary's Hospital"
  2
  (PerPlayer 1)
  Plus
  [Diamond, Square]
  [Arkham]

instance HasModifiersFor env StMarysHospital where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env StMarysHospital where
  getActions iid NonFast (StMarysHospital attrs@Attrs {..}) | locationRevealed =
    do
      baseActions <- getActions iid NonFast attrs
      unused <- getIsUnused iid (ability attrs)
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | unused && iid `member` locationInvestigators && hasActionsRemaining
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (HealDamage (InvestigatorTarget iid) 3)
    _ -> StMarysHospital <$> runMessage msg attrs
