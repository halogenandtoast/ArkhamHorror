module Arkham.Types.Location.Cards.NorthsideTrainStation
  ( NorthsideTrainStation(..)
  , northsideTrainStation
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (northsideTrainStation)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

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

instance HasAbilities env NorthsideTrainStation where
  getAbilities iid window (NorthsideTrainStation attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1 (ActionAbility Nothing $ ActionCost 1)
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env NorthsideTrainStation where
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
