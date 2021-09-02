module Arkham.Types.Location.Cards.LostMemories
  ( lostMemories
  , LostMemories(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (lostMemories)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Query
import qualified Arkham.Types.Timing as Timing

newtype LostMemories = LostMemories LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: LocationCard LostMemories
lostMemories = locationWith
  LostMemories
  Cards.lostMemories
  2
  (PerPlayer 1)
  NoSymbol
  []
  ((revealedSymbolL .~ T)
  . (revealedConnectedSymbolsL .~ setFromList [Square, Moon])
  )

instance HasAbilities LostMemories where
  getAbilities (LostMemories attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
          attrs
          1
          (InvestigatorExists $ You <> InvestigatorWithAnyActionsRemaining)
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env LostMemories where
  runMessage msg l@(LostMemories attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      l <$ push
        (InvestigatorAssignDamage iid source DamageAny 0 actionRemainingCount)
    _ -> LostMemories <$> runMessage msg attrs
