module Arkham.Location.Cards.SlaughteredWoods (
  slaughteredWoods,
  SlaughteredWoods (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (slaughteredWoods)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype SlaughteredWoods = SlaughteredWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

slaughteredWoods :: LocationCard SlaughteredWoods
slaughteredWoods = location SlaughteredWoods Cards.slaughteredWoods 2 (PerPlayer 1)

instance HasAbilities SlaughteredWoods where
  getAbilities (SlaughteredWoods attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (exists $ You <> InvestigatorWithoutActionsRemaining)
            (ForcedAbility $ RevealLocation #after You $ LocationWithId $ toId attrs)
        ]

instance RunMessage SlaughteredWoods where
  runMessage msg l@(SlaughteredWoods attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      l <$ push (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> SlaughteredWoods <$> runMessage msg attrs
