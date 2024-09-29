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
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Ocean))

newtype SecretsOfTheSeaV2 = SecretsOfTheSeaV2 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheSeaV2 :: AgendaCard SecretsOfTheSeaV2
secretsOfTheSeaV2 = agenda (1, A) SecretsOfTheSeaV2 Cards.secretsOfTheSeaV2 (Static 12)

instance HasAbilities SecretsOfTheSeaV2 where
  getAbilities (SecretsOfTheSeaV2 a) = [needsAir a 1]

instance HasModifiersFor SecretsOfTheSeaV2 where
  getModifiersFor (InvestigatorTarget iid) (SecretsOfTheSeaV2 a) = do
    field InvestigatorPlacement iid >>= \case
      AtLocation lid -> do
        isOcean <- lid <=~> LocationWithTrait Ocean
        modified a [AdditionalCostToEnterMatching (LocationWithTrait Ocean) (ActionCost 2) | isOcean]
      _ -> pure []
  getModifiersFor _ _ = pure []

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
