{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MainPath where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Trait
import Arkham.Types.Window
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
  getActions i NonFast (MainPath attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility
               (LocationSource "01149")
               1
               (ActionAbility 1 (Just Action.Resign))
             )
         | getId () i `elem` locationInvestigators
         ]
  getActions i window (MainPath attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env MainPath where
  runMessage msg l@(MainPath attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (LocationSource lid) _ 1
      | lid == locationId && locationRevealed -> l
      <$ unshiftMessage (Resign iid)
    AddConnection lid _ | locationId /= lid -> do
      traits <- HashSet.toList <$> asks (getSet lid)
      if Woods `elem` traits
        then MainPath <$> runMessage
          msg
          (attrs & connectedLocations %~ HashSet.insert lid)
        else MainPath <$> runMessage msg attrs

    _ -> MainPath <$> runMessage msg attrs
