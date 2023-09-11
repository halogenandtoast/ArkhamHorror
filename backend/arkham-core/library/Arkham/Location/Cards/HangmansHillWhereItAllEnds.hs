module Arkham.Location.Cards.HangmansHillWhereItAllEnds (
  hangmansHillWhereItAllEnds,
  HangmansHillWhereItAllEnds (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Trait (Trait (Witch))

newtype HangmansHillWhereItAllEnds = HangmansHillWhereItAllEnds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansHillWhereItAllEnds :: LocationCard HangmansHillWhereItAllEnds
hangmansHillWhereItAllEnds = location HangmansHillWhereItAllEnds Cards.hangmansHillWhereItAllEnds 2 (Static 0)

instance HasAbilities HangmansHillWhereItAllEnds where
  getAbilities (HangmansHillWhereItAllEnds attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage HangmansHillWhereItAllEnds where
  runMessage msg l@(HangmansHillWhereItAllEnds attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let breaches = countLocationBreaches attrs
      act <- selectJust AnyAct
      pushAll
        $ findAndDrawEncounterCard iid (CardWithType EnemyType <> CardWithTrait Witch)
          : ( guard (breaches > 0)
                *> [RemoveBreaches (toTarget attrs) breaches, PlaceBreaches (toTarget act) breaches]
            )
      pure l
    _ -> HangmansHillWhereItAllEnds <$> runMessage msg attrs
