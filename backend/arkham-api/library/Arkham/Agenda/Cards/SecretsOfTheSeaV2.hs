module Arkham.Agenda.Cards.SecretsOfTheSeaV2 (
  SecretsOfTheSeaV2 (..),
  secretsOfTheSeaV2,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card
import Arkham.Cost
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Ocean))

newtype SecretsOfTheSeaV2 = SecretsOfTheSeaV2 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheSeaV2 :: AgendaCard SecretsOfTheSeaV2
secretsOfTheSeaV2 = agenda (1, A) SecretsOfTheSeaV2 Cards.secretsOfTheSeaV2 (Static 9)

instance HasAbilities SecretsOfTheSeaV2 where
  getAbilities (SecretsOfTheSeaV2 a) = [needsAir a 1]

instance HasModifiersFor SecretsOfTheSeaV2 where
  getModifiersFor (SecretsOfTheSeaV2 a) = do
    modifySelect
      a
      (not_ (InVehicleMatching AnyAsset) <> at_ (LocationWithTrait Ocean))
      [AdditionalCostToEnterMatching (LocationWithTrait Ocean) (ActionCost 2)]

instance RunMessage SecretsOfTheSeaV2 where
  runMessage msg a@(SecretsOfTheSeaV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      theTerrorOfDevilReef <- genCard Enemies.theTerrorOfDevilReef_165
      createEnemyAtLocationMatching_
        theTerrorOfDevilReef
        (LocationWithAsset $ assetIs Assets.fishingVessel)
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> SecretsOfTheSeaV2 <$> liftRunMessage msg attrs
