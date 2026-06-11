module Arkham.Asset.Assets.CloakOfTheOuterRealm (cloakOfTheOuterRealm) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype CloakOfTheOuterRealm = CloakOfTheOuterRealm AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloakOfTheOuterRealm :: AssetCard CloakOfTheOuterRealm
cloakOfTheOuterRealm = asset CloakOfTheOuterRealm Cards.cloakOfTheOuterRealm

instance HasAbilities CloakOfTheOuterRealm where
  getAbilities (CloakOfTheOuterRealm a) =
    [ controlled_ a 1
        $ triggered (EnemyWouldEngage #when You NonEliteEnemy) (assetUseCost a Charge 1)
    ]

getEngagingEnemy :: [Window] -> EnemyId
getEngagingEnemy [] = error "missing engaging enemy"
getEngagingEnemy ((windowType -> Window.EnemyWouldEngage _ eid) : _) = eid
getEngagingEnemy (_ : ws) = getEngagingEnemy ws

instance RunMessage CloakOfTheOuterRealm where
  runMessage msg a@(CloakOfTheOuterRealm attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      exhaustThis $ getEngagingEnemy ws
      pure a
    _ -> CloakOfTheOuterRealm <$> liftRunMessage msg attrs
