module Arkham.Location.Cards.HouseInTheReeds_211 (houseInTheReeds_211) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (houseInTheReeds_211)
import Arkham.Location.Helpers (drawCardUnderneathAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype HouseInTheReeds_211 = HouseInTheReeds_211 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseInTheReeds_211 :: LocationCard HouseInTheReeds_211
houseInTheReeds_211 = location HouseInTheReeds_211 Cards.houseInTheReeds_211 1 (PerPlayer 1)

instance HasAbilities HouseInTheReeds_211 where
  getAbilities (HouseInTheReeds_211 a) =
    extendRevealed
      a
      [ drawCardUnderneathAction a
      , mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      ]

instance RunMessage HouseInTheReeds_211 where
  runMessage msg l@(HouseInTheReeds_211 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCardIn iid attrs (card_ $ #enemy <> withTrait Nightgaunt) [FromEncounterDeck]
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      villageCommons <- selectJust $ LocationWithTitle "Village Commons"
      spawnEnemyAt_ card villageCommons
      pure l
    _ -> HouseInTheReeds_211 <$> liftRunMessage msg attrs
