module Arkham.Location.Cards.StreetsOfVenice
  ( streetsOfVenice
  , StreetsOfVenice(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding ( MoveAction )
import Arkham.Message

newtype StreetsOfVenice = StreetsOfVenice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfVenice :: LocationCard StreetsOfVenice
streetsOfVenice = locationWith
  StreetsOfVenice
  Cards.streetsOfVenice
  2
  (Static 2)
  (connectsToL .~ singleton RightOf)

instance HasAbilities StreetsOfVenice where
  getAbilities (StreetsOfVenice attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here $ FastAbility Free
        | locationRevealed attrs
        ]

instance RunMessage StreetsOfVenice where
  runMessage msg l@(StreetsOfVenice attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locations <- selectList $ AccessibleFrom $ LocationWithId $ toId attrs
      case locations of
        [] -> error "No connections?"
        (x : _) -> l <$ push (MoveAction iid x Free False)
    _ -> StreetsOfVenice <$> runMessage msg attrs
