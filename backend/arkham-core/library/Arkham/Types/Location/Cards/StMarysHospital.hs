{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Location.Cards.StMarysHospital
  ( StMarysHospital(..)
  , stMarysHospital
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype StMarysHospital = StMarysHospital Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stMarysHospital :: StMarysHospital
stMarysHospital = StMarysHospital $ baseAttrs
  "01128"
  (LocationName "St. Mary's Hospital" Nothing)
  EncounterSet.TheMidnightMasks
  2
  (PerPlayer 1)
  Plus
  [Diamond, Square]
  [Arkham]

instance HasModifiersFor env StMarysHospital where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PerGame
    }

instance ActionRunner env => HasActions env StMarysHospital where
  getActions iid NonFast (StMarysHospital attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ do
      unused <- getIsUnused iid (ability attrs)
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | unused && iid `member` locationInvestigators
        ]
  getActions iid window (StMarysHospital attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (HealDamage (InvestigatorTarget iid) 3)
    _ -> StMarysHospital <$> runMessage msg attrs
