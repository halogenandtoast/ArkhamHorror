module Arkham.Location.Cards.DressingRoom (
  dressingRoom,
  DressingRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype DressingRoom = DressingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dressingRoom :: LocationCard DressingRoom
dressingRoom = location DressingRoom Cards.dressingRoom 4 (Static 0)

instance HasAbilities DressingRoom where
  getAbilities (DressingRoom attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
        attrs
        1
        ( Here
            <> InvestigatorExists
              (HealableInvestigator (toSource attrs) HorrorType You)
        )
        $ ActionAbility Nothing
        $ ActionCost 3
      | locationRevealed attrs
      ]

instance RunMessage DressingRoom where
  runMessage msg l@(DressingRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mHealHorror <- getHealHorrorMessage attrs 3 iid
      for_ mHealHorror push
      pure l
    _ -> DressingRoom <$> runMessage msg attrs
