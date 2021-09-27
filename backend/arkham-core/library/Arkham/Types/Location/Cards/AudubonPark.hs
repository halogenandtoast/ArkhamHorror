module Arkham.Types.Location.Cards.AudubonPark where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (audubonPark)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyEvaded)
import Arkham.Types.Timing qualified as Timing

newtype AudubonPark = AudubonPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

audubonPark :: LocationCard AudubonPark
audubonPark = location
  AudubonPark
  Cards.audubonPark
  3
  (PerPlayer 1)
  Squiggle
  [Triangle, Squiggle]

instance HasAbilities AudubonPark where
  getAbilities (AudubonPark attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here
        $ ForcedAbility
        $ EnemyEvaded Timing.When You
        $ EnemyAt
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env AudubonPark where
  runMessage msg l@(AudubonPark attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (RandomDiscard iid)
    _ -> AudubonPark <$> runMessage msg attrs
