module Arkham.Types.Location.Cards.StreetsOfVenice
  ( streetsOfVenice
  , StreetsOfVenice(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (MoveAction)
import Arkham.Types.Message

newtype StreetsOfVenice = StreetsOfVenice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfVenice :: LocationCard StreetsOfVenice
streetsOfVenice = locationWith
  StreetsOfVenice
  Cards.streetsOfVenice
  2
  (Static 2)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities StreetsOfVenice where
  getAbilities (StreetsOfVenice attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here $ FastAbility Free
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage env StreetsOfVenice where
  runMessage msg l@(StreetsOfVenice attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locations <- selectList AccessibleLocation
      case locations of
        [] -> error "No connections?"
        (x : _) -> l <$ push (MoveAction iid x Free False)
    _ -> StreetsOfVenice <$> runMessage msg attrs
