{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MainPath where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype MainPath = MainPath Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mainPath :: MainPath
mainPath =
  MainPath $ (baseAttrs "01149" "Main Path" 2 (Static 0) Squiggle [Square, Plus]
             )
    { locationTraits = HashSet.fromList [Woods]
    }

instance (IsInvestigator investigator) => HasActions env investigator MainPath where
  getActions i window (MainPath attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env MainPath where
  runMessage msg (MainPath attrs@Attrs {..}) = case msg of
    AddConnection lid _ | locationId /= lid -> do
      traits <- HashSet.toList <$> asks (getSet lid)
      if Woods `elem` traits
        then MainPath <$> runMessage
          msg
          (attrs & connectedLocations %~ HashSet.insert lid)
        else MainPath <$> runMessage msg attrs

    _ -> MainPath <$> runMessage msg attrs
