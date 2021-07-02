module Arkham.Types.Location.Cards.FauborgMarigny
  ( FauborgMarigny(..)
  , fauborgMarigny
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (fauborgMarigny)
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype FauborgMarigny = FauborgMarigny LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fauborgMarigny :: LocationId -> FauborgMarigny
fauborgMarigny = FauborgMarigny . baseAttrs
  Cards.fauborgMarigny
  4
  (Static 0)
  Squiggle
  [Triangle, Squiggle]

instance HasModifiersFor env FauborgMarigny where
  getModifiersFor _ (InvestigatorTarget iid) (FauborgMarigny attrs) =
    pure $ toModifiers
      attrs
      [ ReduceCostOfCardType AssetType 1
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FauborgMarigny where
  getActions = withResignAction

instance (LocationRunner env) => RunMessage env FauborgMarigny where
  runMessage msg (FauborgMarigny attrs) =
    FauborgMarigny <$> runMessage msg attrs
