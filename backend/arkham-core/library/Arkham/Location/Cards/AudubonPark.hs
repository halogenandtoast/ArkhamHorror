module Arkham.Location.Cards.AudubonPark where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Discard
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( audubonPark )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype AudubonPark = AudubonPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

audubonPark :: LocationCard AudubonPark
audubonPark = location AudubonPark Cards.audubonPark 3 (PerPlayer 1)

instance HasAbilities AudubonPark where
  getAbilities (AudubonPark attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here
          $ ForcedAbility
          $ EnemyEvaded Timing.When You
          $ enemyAt
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage AudubonPark where
  runMessage msg l@(AudubonPark attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ toMessage $ randomDiscard iid $ toAbilitySource attrs 1
      pure l
    _ -> AudubonPark <$> runMessage msg attrs
