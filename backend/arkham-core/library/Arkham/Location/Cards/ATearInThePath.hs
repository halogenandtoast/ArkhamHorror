module Arkham.Location.Cards.ATearInThePath (
  aTearInThePath,
  ATearInThePath (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (aTearInThePath)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ATearInThePath = ATearInThePath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInThePath :: LocationCard ATearInThePath
aTearInThePath = location ATearInThePath Cards.aTearInThePath 3 (PerPlayer 1)

instance HasAbilities ATearInThePath where
  getAbilities (ATearInThePath attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
        attrs
        1
        (InvestigatorExists $ You <> InvestigatorWithoutActionsRemaining)
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance RunMessage ATearInThePath where
  runMessage msg l@(ATearInThePath attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l <$ push (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> ATearInThePath <$> runMessage msg attrs
