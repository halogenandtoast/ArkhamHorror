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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trainTracks :: LocationCard TrainTracks
trainTracks = location TrainTracks Cards.trainTracks 3 (PerPlayer 1)

instance HasModifiersFor TrainTracks where
  getModifiersFor (LocationTarget lid) (TrainTracks a) = do
    isNorthside <- lid <=~> locationIs Cards.northside
    pure $
      toModifiers
        a
        [ ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)
        | isNorthside
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities TrainTracks where
  getAbilities (TrainTracks attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (PlayerLimit PerGame 1) $
          restrictedAbility attrs 1 Here $
            ActionAbility Nothing $
              ActionCost 1
                <> ClueCost (Static 1)
      ]

instance RunMessage TrainTracks where
  runMessage msg l@(TrainTracks attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 4
      push drawing
      pure l
    _ -> TrainTracks <$> runMessage msg attrs
