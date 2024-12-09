module Arkham.Agenda.Cards.SecretsOfTheSeaV1 (
  SecretsOfTheSeaV1 (..),
  secretsOfTheSeaV1,
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

newtype SecretsOfTheSeaV1 = SecretsOfTheSeaV1 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheSeaV1 :: AgendaCard SecretsOfTheSeaV1
secretsOfTheSeaV1 = agenda (1, A) SecretsOfTheSeaV1 Cards.secretsOfTheSeaV1 (Static 9)

instance HasAbilities SecretsOfTheSeaV1 where
  getAbilities (SecretsOfTheSeaV1 a) = [needsAir a 1]

instance HasModifiersFor SecretsOfTheSeaV1 where
  getModifiersFor (SecretsOfTheSeaV1 a) = do
    modifySelect
      a
      (not_ (InVehicleMatching AnyAsset) <> at_ (LocationWithTrait Ocean))
      [AdditionalCostToEnterMatching (LocationWithTrait Ocean) (ActionCost 2)]

instance RunMessage SecretsOfTheSeaV1 where
  runMessage msg a@(SecretsOfTheSeaV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      theTerrorOfDevilReef <- genCard Enemies.theTerrorOfDevilReef_164
      createEnemyAtLocationMatching_
        theTerrorOfDevilReef
        (LocationWithAsset $ assetIs Assets.fishingVessel)
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> SecretsOfTheSeaV1 <$> liftRunMessage msg attrs
