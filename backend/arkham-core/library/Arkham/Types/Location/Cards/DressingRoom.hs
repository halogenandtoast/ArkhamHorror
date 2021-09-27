module Arkham.Types.Location.Cards.DressingRoom
  ( dressingRoom
  , DressingRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Target

newtype DressingRoom = DressingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dressingRoom :: LocationCard DressingRoom
dressingRoom =
  location DressingRoom Cards.dressingRoom 4 (Static 0) Moon [Diamond]

instance HasAbilities DressingRoom where
  getAbilities (DressingRoom attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 3
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env DressingRoom where
  runMessage msg l@(DressingRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (HealHorror (InvestigatorTarget iid) 3)
    _ -> DressingRoom <$> runMessage msg attrs
