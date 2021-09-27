module Arkham.Types.Location.Cards.BoxOffice
  ( boxOffice
  , BoxOffice(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey

newtype BoxOffice = BoxOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxOffice :: LocationCard BoxOffice
boxOffice = location BoxOffice Cards.boxOffice 2 (Static 0) Plus [Triangle]

instance HasAbilities BoxOffice where
  getAbilities (BoxOffice attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env BoxOffice where
  runMessage msg l@(BoxOffice attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll [TakeResources iid 5 False, Remember StoleFromTheBoxOffice]
    _ -> BoxOffice <$> runMessage msg attrs
