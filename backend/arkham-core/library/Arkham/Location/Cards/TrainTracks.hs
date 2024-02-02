module Arkham.Location.Cards.TrainTracks (
  trainTracks,
  TrainTracks (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype TrainTracks = TrainTracks LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

trainTracks :: LocationCard TrainTracks
trainTracks = location TrainTracks Cards.trainTracks 3 (PerPlayer 1)

instance HasModifiersFor TrainTracks where
  getModifiersFor (LocationTarget lid) (TrainTracks a) = do
    isNorthside <- lid <=~> locationIs Cards.northside
    pure $ toModifiers a [ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a) | isNorthside]
  getModifiersFor _ _ = pure []

instance HasAbilities TrainTracks where
  getAbilities (TrainTracks attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ restrictedAbility attrs 1 Here
            $ ActionAbility []
            $ ActionCost 1
            <> ClueCost (Static 1)
        ]

instance RunMessage TrainTracks where
  runMessage msg l@(TrainTracks attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 4
      pure l
    _ -> TrainTracks <$> runMessage msg attrs
