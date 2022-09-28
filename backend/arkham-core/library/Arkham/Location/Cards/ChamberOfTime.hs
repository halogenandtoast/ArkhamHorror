module Arkham.Location.Cards.ChamberOfTime
  ( chamberOfTime
  , ChamberOfTime(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Timing qualified as Timing

newtype ChamberOfTime = ChamberOfTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTime :: LocationCard ChamberOfTime
chamberOfTime = locationWith
  ChamberOfTime
  Cards.chamberOfTime
  4
  (PerPlayer 2)
  (connectsToL .~ singleton RightOf)

instance HasAbilities ChamberOfTime where
  getAbilities (ChamberOfTime attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ PutLocationIntoPlay Timing.After Anyone
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage ChamberOfTime where
  runMessage msg l@(ChamberOfTime attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      relicOfAges <- getSetAsideCard Assets.relicOfAgesADeviceOfSomeSort
      pushAll
        [ CreateAssetAt relicOfAges (AttachedToLocation $ toId attrs)
        , PlaceDoom (toTarget attrs) 1
        ]
      pure l
    _ -> ChamberOfTime <$> runMessage msg attrs
