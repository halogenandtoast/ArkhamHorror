module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.BanquetHallRuined (banquetHallRuined) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Geist))

newtype BanquetHallRuined = BanquetHallRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banquetHallRuined :: LocationCard BanquetHallRuined
banquetHallRuined = location BanquetHallRuined Cards.banquetHallRuined 2 (Static 0)

instance HasAbilities BanquetHallRuined where
  getAbilities (BanquetHallRuined a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "banquetHallRuined.evadeGeist"
      $ groupLimit PerRound
      $ noAOO
      $ restricted
        a
        1
        (Here <> InvestigatorExists (You <> investigatorWithHospitality) <> exists (EnemyWithTrait Geist))
        actionAbility

instance RunMessage BanquetHallRuined where
  runMessage msg l@(BanquetHallRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      geists <- select $ EnemyWithTrait Geist
      chooseTargetM iid geists $ automaticallyEvadeEnemy iid
      pure l
    _ -> BanquetHallRuined <$> liftRunMessage msg attrs
