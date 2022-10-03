module Arkham.Location.Cards.AudubonPark where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( audubonPark )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EnemyEvaded )
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
          $ EnemyAt
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage AudubonPark where
  runMessage msg l@(AudubonPark attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (RandomDiscard iid)
    _ -> AudubonPark <$> runMessage msg attrs
