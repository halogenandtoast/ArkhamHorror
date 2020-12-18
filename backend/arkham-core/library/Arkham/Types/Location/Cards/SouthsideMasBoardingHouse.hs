{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.SouthsideMasBoardingHouse
  ( SouthsideMasBoardingHouse(..)
  , southsideMasBoardingHouse
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
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
  EncounterSet.TheMidnightMasks
  2
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]
  [Arkham]

instance HasModifiersFor env SouthsideMasBoardingHouse where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env SouthsideMasBoardingHouse where
  getActions iid NonFast (SouthsideMasBoardingHouse attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions iid NonFast attrs
      unused <- getIsUnused iid (ability attrs)
      canAffordActions <- getCanAffordCost
        iid
        (toSource attrs)
        (ActionCost 1 Nothing locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | unused && iid `member` locationInvestigators && canAffordActions
           ]
  getActions iid window (SouthsideMasBoardingHouse attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Ally])
    _ -> SouthsideMasBoardingHouse <$> runMessage msg attrs
