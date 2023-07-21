module Arkham.Location.Cards.BoxOffice (
  boxOffice,
  BoxOffice (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.ScenarioLogKey

newtype BoxOffice = BoxOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxOffice :: LocationCard BoxOffice
boxOffice = location BoxOffice Cards.boxOffice 2 (Static 0)

instance HasAbilities BoxOffice where
  getAbilities (BoxOffice attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (GroupLimit PerGame 1) $
        restrictedAbility attrs 1 Here $
          ActionAbility Nothing $
            ActionCost 1
      | locationRevealed attrs
      ]

instance RunMessage BoxOffice where
  runMessage msg l@(BoxOffice attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l <$ pushAll [TakeResources iid 5 (toAbilitySource attrs 1) False, Remember StoleFromTheBoxOffice]
    _ -> BoxOffice <$> runMessage msg attrs
