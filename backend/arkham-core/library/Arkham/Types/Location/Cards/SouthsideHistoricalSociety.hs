{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Location.Cards.SouthsideHistoricalSociety
  ( SouthsideHistoricalSociety(..)
  , southsideHistoricalSociety
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype SouthsideHistoricalSociety = SouthsideHistoricalSociety Attrs
  deriving newtype (Show, ToJSON, FromJSON)

southsideHistoricalSociety :: SouthsideHistoricalSociety
southsideHistoricalSociety = SouthsideHistoricalSociety $ baseAttrs
  "01126"
  (LocationName "Southside" $ Just "Historical Society")
  EncounterSet.TheMidnightMasks
  3
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]
  [Arkham]

instance HasModifiersFor env SouthsideHistoricalSociety where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PerGame
    }

instance ActionRunner env => HasActions env SouthsideHistoricalSociety where
  getActions iid NonFast (SouthsideHistoricalSociety attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions iid NonFast attrs
      unused <- getIsUnused iid (ability attrs)
      canAffordActions <- getCanAffordCost
        iid
        (toSource attrs)
        Nothing
        (ActionCost 1)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | unused && iid `member` locationInvestigators && canAffordActions
           ]
  getActions iid window (SouthsideHistoricalSociety attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env SouthsideHistoricalSociety where
  runMessage msg l@(SouthsideHistoricalSociety attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (DrawCards iid 3 False)
    _ -> SouthsideHistoricalSociety <$> runMessage msg attrs
