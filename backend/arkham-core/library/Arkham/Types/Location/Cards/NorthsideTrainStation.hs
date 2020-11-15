{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.NorthsideTrainStation
  ( NorthsideTrainStation(..)
  , northsideTrainStation
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype NorthsideTrainStation = NorthsideTrainStation Attrs
  deriving newtype (Show, ToJSON, FromJSON)

northsideTrainStation :: NorthsideTrainStation
northsideTrainStation = NorthsideTrainStation
  (baseAttrs
    "50028"
    "Northside"
    EncounterSet.TheMidnightMasks
    2
    (PerPlayer 1)
    T
    [Diamond, Triangle]
    [Arkham]
  )

instance HasModifiersFor env NorthsideTrainStation where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env NorthsideTrainStation where
  getActions iid NonFast (NorthsideTrainStation attrs@Attrs {..})
    | locationRevealed = do
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

instance LocationRunner env => RunMessage env NorthsideTrainStation where
  runMessage msg l@(NorthsideTrainStation attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationIds <- asks $ setToList . getSet [Arkham]
      l <$ unshiftMessage
        (chooseOne iid [ MoveTo iid lid | lid <- locationIds ])
    _ -> NorthsideTrainStation <$> runMessage msg attrs
