module Arkham.Types.Location.Cards.MainPath
  ( MainPath(..)
  , mainPath
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MainPath = MainPath LocationAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

mainPath :: MainPath
mainPath = MainPath $ baseAttrs
  "01149"
  (Name "Main Path" Nothing)
  EncounterSet.TheDevourerBelow
  2
  (Static 0)
  Squiggle
  [Square, Plus]
  [Woods]

instance HasModifiersFor env MainPath where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MainPath where
  getActions iid NonFast (MainPath attrs@LocationAttrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource attrs)
            1
            (ActionAbility (Just Action.Resign) (ActionCost 1))
          )
      | iid `member` locationInvestigators
      ]
  getActions i window (MainPath attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env MainPath where
  runMessage msg l@(MainPath attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (Resign iid)
    AddConnection lid _ | locationId /= lid -> do
      isWoods <- member Woods <$> getSet lid
      if isWoods
        then MainPath
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else MainPath <$> runMessage msg attrs
    _ -> MainPath <$> runMessage msg attrs
