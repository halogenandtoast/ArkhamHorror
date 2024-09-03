module Arkham.Location.Cards.TrainTracks (trainTracks, TrainTracks (..)) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TrainTracks = TrainTracks LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trainTracks :: LocationCard TrainTracks
trainTracks = location TrainTracks Cards.trainTracks 3 (PerPlayer 1)

instance HasModifiersFor TrainTracks where
  getModifiersFor (LocationTarget lid) (TrainTracks a) = do
    isNorthside <- lid <=~> locationIs Cards.northside
    pure $ toModifiers a [ConnectedToWhen (LocationWithId lid) (be a) | isNorthside]
  getModifiersFor _ _ = pure []

instance HasAbilities TrainTracks where
  getAbilities (TrainTracks attrs) =
    withBaseAbilities
      attrs
      [ playerLimit PerGame $ restrictedAbility attrs 1 Here $ actionAbilityWithCost (ClueCost $ Static 1)
      ]

instance RunMessage TrainTracks where
  runMessage msg l@(TrainTracks attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (attrs.ability 1) 4
      pure l
    _ -> TrainTracks <$> runMessage msg attrs
