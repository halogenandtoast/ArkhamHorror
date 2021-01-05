module Arkham.Types.Location.Cards.FauborgMarigny
  ( FauborgMarigny(..)
  , fauborgMarigny
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype FauborgMarigny = FauborgMarigny Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fauborgMarigny :: FauborgMarigny
fauborgMarigny = FauborgMarigny $ baseAttrs
  "81012"
  (LocationName "Faurborg Marigny" Nothing)
  EncounterSet.CurseOfTheRougarou
  4
  (Static 0)
  Squiggle
  [Triangle, Squiggle]
  [Riverside]

instance HasModifiersFor env FauborgMarigny where
  getModifiersFor _ (InvestigatorTarget iid) (FauborgMarigny attrs) =
    pure $ toModifiers
      attrs
      [ ReduceCostOfCardType AssetType 1
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility (Just Action.Resign) (ActionCost 1))

instance ActionRunner env => HasActions env FauborgMarigny where
  getActions iid NonFast (FauborgMarigny attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | iid `member` locationInvestigators
      ]
  getActions i window (FauborgMarigny attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env FauborgMarigny where
  runMessage msg l@(FauborgMarigny attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (Resign iid)
    _ -> FauborgMarigny <$> runMessage msg attrs
