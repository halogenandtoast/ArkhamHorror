module Arkham.Location.Cards.ATearInThePath (aTearInThePath) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (aTearInThePath)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ATearInThePath = ATearInThePath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInThePath :: LocationCard ATearInThePath
aTearInThePath = location ATearInThePath Cards.aTearInThePath 3 (PerPlayer 1)

instance HasAbilities ATearInThePath where
  getAbilities (ATearInThePath attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (youExist InvestigatorWithoutActionsRemaining)
      $ forced
      $ RevealLocation #after You (be attrs)

instance RunMessage ATearInThePath where
  runMessage msg l@(ATearInThePath attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push (InvestigatorAssignDamage iid source DamageAny 2 0)
      pure l
    _ -> ATearInThePath <$> runMessage msg attrs
