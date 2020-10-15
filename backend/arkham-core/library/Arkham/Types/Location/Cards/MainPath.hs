{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.MainPath where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MainPath = MainPath Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mainPath :: MainPath
mainPath = MainPath
  $ baseAttrs "01149" "Main Path" 2 (Static 0) Squiggle [Square, Plus] [Woods]

instance HasModifiersFor env investigator MainPath where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator MainPath where
  getActions i NonFast (MainPath attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility
               (toSource attrs)
               1
               (ActionAbility 1 (Just Action.Resign))
             )
         | atLocation i attrs
           && hasActionsRemaining i (Just Action.Resign) locationTraits
         ]
  getActions i window (MainPath attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env MainPath where
  runMessage msg l@(MainPath attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source && locationRevealed ->
      l <$ unshiftMessage (Resign iid)
    AddConnection lid _ | locationId /= lid -> do
      isWoods <- asks $ member Woods . getSet lid
      if isWoods
        then MainPath
          <$> runMessage msg (attrs & connectedLocations %~ insertSet lid)
        else MainPath <$> runMessage msg attrs
    _ -> MainPath <$> runMessage msg attrs
