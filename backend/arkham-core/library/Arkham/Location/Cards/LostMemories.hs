module Arkham.Location.Cards.LostMemories
  ( lostMemories
  , LostMemories(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (lostMemories)
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message hiding (RevealLocation)
import Arkham.Query
import Arkham.Timing qualified as Timing

newtype LostMemories = LostMemories LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: LocationCard LostMemories
lostMemories = locationWithRevealedSideConnections
  LostMemories
  Cards.lostMemories
  2
  (PerPlayer 1)
  NoSymbol
  []
  T
  [Square, Moon]

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

instance LocationRunner env => RunMessage LostMemories where
  runMessage msg l@(LostMemories attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      l <$ push
        (InvestigatorAssignDamage iid source DamageAny 0 actionRemainingCount)
    _ -> LostMemories <$> runMessage msg attrs
