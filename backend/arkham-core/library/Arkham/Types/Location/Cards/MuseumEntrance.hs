module Arkham.Types.Location.Cards.MuseumEntrance
  ( museumEntrance
  , MuseumEntrance(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MuseumEntrance = MuseumEntrance Attrs
  deriving newtype (Show, ToJSON, FromJSON)

museumEntrance :: MuseumEntrance
museumEntrance = MuseumEntrance $ baseAttrs
  "02126"
  (LocationName "Museum Entrance" Nothing)
  EncounterSet.TheMiskatonicMuseum
  3
  (Static 2)
  Circle
  [Square]
  (singleton Miskatonic)

instance HasModifiersFor env MuseumEntrance where
  getModifiersFor _ (InvestigatorTarget iid) (MuseumEntrance attrs) =
    pure $ toModifiers
      attrs
      [ CannotGainResources | iid `member` locationInvestigators attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env MuseumEntrance where
  getActions iid NonFast (MuseumEntrance attrs@Attrs {..}) | locationRevealed =
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
  getActions i window (MuseumEntrance attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env MuseumEntrance where
  runMessage msg l@(MuseumEntrance attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (Resign iid)
    _ -> MuseumEntrance <$> runMessage msg attrs
