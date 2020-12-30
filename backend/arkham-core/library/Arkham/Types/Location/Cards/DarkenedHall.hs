{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DarkenedHall
  ( darkenedHall
  , DarkenedHall(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait hiding (Cultist)

newtype DarkenedHall = DarkenedHall Attrs
  deriving newtype (Show, ToJSON, FromJSON)

darkenedHall :: DarkenedHall
darkenedHall = DarkenedHall $ (baseAttrs
                                "02074"
                                (LocationName "Darkened Hall" Nothing)
                                EncounterSet.TheHouseAlwaysWins
                                4
                                (Static 0)
                                Diamond
                                [Triangle]
                                [CloverClub]
                              )
  { locationRevealedConnectedSymbols = setFromList
    [Triangle, T, Hourglass, Plus, Squiggle]
  }

instance HasModifiersFor env DarkenedHall where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DarkenedHall where
  getActions iid window (DarkenedHall attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DarkenedHall where
  runMessage msg (DarkenedHall attrs@Attrs {..}) = case msg of
    RevealLocation _ lid | lid == locationId -> do
      unshiftMessages
        [PlaceLocation "02075", PlaceLocation "02076", PlaceLocation "02077"]
      DarkenedHall <$> runMessage msg attrs
    _ -> DarkenedHall <$> runMessage msg attrs
