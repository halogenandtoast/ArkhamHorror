module Arkham.Location.Cards.DressingRoom
  ( dressingRoom
  , DressingRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype DressingRoom = DressingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dressingRoom :: LocationCard DressingRoom
dressingRoom = location DressingRoom Cards.dressingRoom 4 (Static 0)

instance HasAbilities DressingRoom where
  getAbilities (DressingRoom attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Here <> InvestigatorExists (HealableInvestigator HorrorType You))
      $ ActionAbility Nothing
      $ ActionCost 3
    | locationRevealed attrs
    ]

instance RunMessage DressingRoom where
  runMessage msg l@(DressingRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (HealHorror (InvestigatorTarget iid) (toSource attrs) 3)
    _ -> DressingRoom <$> runMessage msg attrs
