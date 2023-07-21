module Arkham.Location.Cards.HouseInTheReeds_211 (
  houseInTheReeds_211,
  HouseInTheReeds_211 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (houseInTheReeds_211)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype HouseInTheReeds_211 = HouseInTheReeds_211 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseInTheReeds_211 :: LocationCard HouseInTheReeds_211
houseInTheReeds_211 =
  location HouseInTheReeds_211 Cards.houseInTheReeds_211 1 (PerPlayer 1)

instance HasModifiersFor HouseInTheReeds_211

instance HasAbilities HouseInTheReeds_211 where
  getAbilities (HouseInTheReeds_211 x) =
    let rest = withDrawCardUnderneathAction x
    in  rest
          <> [ mkAbility x 1 $
              ForcedAbility $
                RevealLocation Timing.After Anyone $
                  LocationWithId $
                    toId x
             | locationRevealed x
             ]

instance RunMessage HouseInTheReeds_211 where
  runMessage msg l@(HouseInTheReeds_211 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $
        FindEncounterCard iid (toTarget attrs) [FromEncounterDeck] $
          CardWithType EnemyType
            <> CardWithTrait Nightgaunt
      pure l
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      villageCommonsId <- selectJust $ LocationWithTitle "Village Commons"
      l <$ push (SpawnEnemyAt (EncounterCard card) villageCommonsId)
    _ -> HouseInTheReeds_211 <$> runMessage msg attrs
