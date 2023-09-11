module Arkham.Location.Cards.SilverTwilightLodgeWhereItAllEnds (
  silverTwilightLodgeWhereItAllEnds,
  SilverTwilightLodgeWhereItAllEnds (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Trait (Trait (SilverTwilight))

newtype SilverTwilightLodgeWhereItAllEnds = SilverTwilightLodgeWhereItAllEnds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightLodgeWhereItAllEnds :: LocationCard SilverTwilightLodgeWhereItAllEnds
silverTwilightLodgeWhereItAllEnds = location SilverTwilightLodgeWhereItAllEnds Cards.silverTwilightLodgeWhereItAllEnds 2 (Static 0)

instance HasAbilities SilverTwilightLodgeWhereItAllEnds where
  getAbilities (SilverTwilightLodgeWhereItAllEnds attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage SilverTwilightLodgeWhereItAllEnds where
  runMessage msg l@(SilverTwilightLodgeWhereItAllEnds attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let breaches = countLocationBreaches attrs
      act <- selectJust AnyAct
      pushAll
        $ findAndDrawEncounterCard iid (CardWithType EnemyType <> CardWithTrait SilverTwilight)
          : ( guard (breaches > 0)
                *> [RemoveBreaches (toTarget attrs) breaches, PlaceBreaches (toTarget act) breaches]
            )
      pure l
    _ -> SilverTwilightLodgeWhereItAllEnds <$> runMessage msg attrs
