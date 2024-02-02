module Arkham.Location.Cards.NorthsideTrainStation (
  NorthsideTrainStation (..),
  northsideTrainStation,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (northsideTrainStation)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Trait

newtype NorthsideTrainStation = NorthsideTrainStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

northsideTrainStation :: LocationCard NorthsideTrainStation
northsideTrainStation =
  location NorthsideTrainStation Cards.northsideTrainStation 2 (PerPlayer 1)

instance HasAbilities NorthsideTrainStation where
  getAbilities (NorthsideTrainStation attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility attrs 1 Here
          $ ActionAbility []
          $ ActionCost 1
        | locationRevealed attrs
        ]

instance RunMessage NorthsideTrainStation where
  runMessage msg l@(NorthsideTrainStation attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationIds <- selectList $ LocationWithTrait Arkham
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel lid [MoveTo $ move (toSource attrs) iid lid]
          | lid <- locationIds
          ]
      pure l
    _ -> NorthsideTrainStation <$> runMessage msg attrs
