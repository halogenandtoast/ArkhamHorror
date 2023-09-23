module Arkham.Location.Cards.LostMemories (
  lostMemories,
  LostMemories (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (lostMemories)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype LostMemories = LostMemories LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: LocationCard LostMemories
lostMemories = location LostMemories Cards.lostMemories 2 (PerPlayer 1)

instance HasAbilities LostMemories where
  getAbilities (LostMemories attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
          attrs
          1
          (InvestigatorExists $ You <> InvestigatorWithAnyActionsRemaining)
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage LostMemories where
  runMessage msg l@(LostMemories attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      l
        <$ push
          (InvestigatorAssignDamage iid source DamageAny 0 actionRemainingCount)
    _ -> LostMemories <$> runMessage msg attrs
