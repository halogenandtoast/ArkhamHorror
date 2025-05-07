module Arkham.Location.Cards.CongregationalChurch_208 (congregationalChurch_208) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (congregationalChurch_208)
import Arkham.Location.Helpers (drawCardUnderneathAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype CongregationalChurch_208 = CongregationalChurch_208 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_208 :: LocationCard CongregationalChurch_208
congregationalChurch_208 = location CongregationalChurch_208 Cards.congregationalChurch_208 1 (PerPlayer 1)

instance HasAbilities CongregationalChurch_208 where
  getAbilities (CongregationalChurch_208 a) =
    extendRevealed
      a
      [ drawCardUnderneathAction a
      , mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      ]

instance RunMessage CongregationalChurch_208 where
  runMessage msg l@(CongregationalChurch_208 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCardIn iid (toTarget attrs) (#enemy <> CardWithTrait Humanoid) [#deck]
      pure l
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      villageCommons <- selectJust $ LocationWithTitle "Village Commons"
      createEnemyAt_ card villageCommons
      pure l
    _ -> CongregationalChurch_208 <$> liftRunMessage msg attrs
