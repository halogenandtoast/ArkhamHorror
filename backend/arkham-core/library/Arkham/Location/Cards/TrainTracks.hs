module Arkham.Location.Cards.TrainTracks
  ( trainTracks
  , TrainTracks(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype TrainTracks = TrainTracks LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trainTracks :: LocationCard TrainTracks
trainTracks = location TrainTracks Cards.trainTracks 3 (PerPlayer 1)

instance HasModifiersFor TrainTracks where
  getModifiersFor (LocationTarget lid) (TrainTracks a) = do
    isNorthside <- lid <=~> locationIs Cards.northside
    pure $ toModifiers
      a
      [ ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)
      | isNorthside
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities TrainTracks where
  getAbilities (TrainTracks attrs) = withBaseAbilities
    attrs
    [ limitedAbility (PlayerLimit PerGame 1)
      $ restrictedAbility attrs 1 Here
      $ ActionAbility Nothing
      $ ActionCost 1
      <> ClueCost 1
    ]

instance RunMessage TrainTracks where
  runMessage msg l@(TrainTracks attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ DrawCards iid 4 False
      pure l
    _ -> TrainTracks <$> runMessage msg attrs
