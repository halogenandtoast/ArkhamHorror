module Arkham.Agenda.Cards.TheTrueCulpritV1 (theTrueCulpritV1) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher

newtype TheTrueCulpritV1 = TheTrueCulpritV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV1 :: AgendaCard TheTrueCulpritV1
theTrueCulpritV1 = agenda (3, A) TheTrueCulpritV1 Cards.theTrueCulpritV1 (Static 6)

instance HasAbilities TheTrueCulpritV1 where
  getAbilities (TheTrueCulpritV1 attrs) =
    guard (onSide A attrs)
      *> [ controlled
             (proxied (assetIs Cards.alienDevice) attrs)
             1
             (exists (enemyIs Cards.vengefulSpecter <> ExhaustedEnemy))
             (actionAbilityWithCost $ AssetClueCost "Alien Device" (assetIs Cards.alienDevice) $ Static 2)
         , mkAbility attrs 2
             $ Objective
             $ forced
             $ EnemyDefeated #after Anyone ByAny
             $ enemyIs Cards.vengefulSpecter
         ]

instance RunMessage TheTrueCulpritV1 where
  runMessage msg a@(TheTrueCulpritV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
      vengefulSpecter <- selectJust $ enemyIs Cards.vengefulSpecter
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2 vengefulSpecter
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      push $ if means == AgendaAdvancedWithDoom then R2 else R1
      pure a
    _ -> TheTrueCulpritV1 <$> liftRunMessage msg attrs
