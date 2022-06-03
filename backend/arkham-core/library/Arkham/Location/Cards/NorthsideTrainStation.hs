module Arkham.Location.Cards.NorthsideTrainStation
  ( NorthsideTrainStation(..)
  , northsideTrainStation
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (northsideTrainStation)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype NorthsideTrainStation = NorthsideTrainStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northsideTrainStation :: LocationCard NorthsideTrainStation
northsideTrainStation = location
  NorthsideTrainStation
  Cards.northsideTrainStation
  2
  (PerPlayer 1)
  T
  [Diamond, Triangle]

instance HasAbilities NorthsideTrainStation where
  getAbilities (NorthsideTrainStation attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage NorthsideTrainStation where
  runMessage msg l@(NorthsideTrainStation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationIds <- getSetList [Arkham]
      l <$ push
        (chooseOne
          iid
          [ TargetLabel (LocationTarget lid) [MoveTo (toSource attrs) iid lid]
          | lid <- locationIds
          ]
        )
    _ -> NorthsideTrainStation <$> runMessage msg attrs
