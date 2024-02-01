module Arkham.Location.Cards.UprootedWoods (
  uprootedWoods,
  UprootedWoods (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (uprootedWoods)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype UprootedWoods = UprootedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

uprootedWoods :: LocationCard UprootedWoods
uprootedWoods = location UprootedWoods Cards.uprootedWoods 2 (PerPlayer 1)

instance HasAbilities UprootedWoods where
  getAbilities (UprootedWoods attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
          attrs
          1
          (InvestigatorExists $ You <> InvestigatorWithoutActionsRemaining)
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage UprootedWoods where
  runMessage msg l@(UprootedWoods attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ DiscardTopOfDeck iid 5 (toAbilitySource attrs 1) Nothing
      pure l
    _ -> UprootedWoods <$> runMessage msg attrs
