module Arkham.Enemy.Cards.HuntingParasite (huntingParasite) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HuntingParasite = HuntingParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingParasite :: EnemyCard HuntingParasite
huntingParasite =
  enemyWith HuntingParasite Cards.huntingParasite
    (spawnAtL ?~ SpawnAtFirst [SpawnAt connectingEmpty, SpawnAt connecting])
    & setPreyIsOnlyBearer
 where
  connecting = ConnectedLocation NotForMovement
  connectingEmpty = connecting <> EmptyLocation

instance HasAbilities HuntingParasite where
  getAbilities (HuntingParasite a) =
    extend1 a $ mkAbility a 1 $ forced $ PhaseEnds #when #enemy

instance RunMessage HuntingParasite where
  runMessage msg e@(HuntingParasite attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \loc -> do
        assets <- select $ AssetAt (LocationWithId loc) <> AssetCardMatch IsPlayerCard
        events <- select $ EventAt (LocationWithId loc) <> EventCardMatch IsPlayerCard
        let cards = map toTarget assets <> map toTarget events
        lead <- getLead
        chooseTargetM lead cards \target -> placeDoom (attrs.ability 1) target 1
      toDiscard (attrs.ability 1) attrs
      pure e
    _ -> HuntingParasite <$> liftRunMessage msg attrs
