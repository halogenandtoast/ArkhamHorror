module Arkham.Agenda.Cards.TheTrueCulpritV6 (theTrueCulpritV6) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher

newtype TheTrueCulpritV6 = TheTrueCulpritV6 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV6 :: AgendaCard TheTrueCulpritV6
theTrueCulpritV6 = agenda (3, A) TheTrueCulpritV6 Cards.theTrueCulpritV6 (Static 6)

instance HasAbilities TheTrueCulpritV6 where
  getAbilities (TheTrueCulpritV6 attrs) =
    guard (onSide A attrs)
      *> [ restricted
            (proxied (assetIs Cards.timeWornLocket) attrs)
            1
            ControlsThis
            $ ReactionAbility
              (EnemyWouldAttack #when (colocatedWithMatch You) AnyEnemyAttack $ enemyIs Cards.hotelManager)
              (AssetClueCost "Time-worn Locket" (assetIs Cards.timeWornLocket) $ Static 2)
         , mkAbility attrs 2
            $ Objective
            $ forced
            $ EnemyDefeated #after Anyone ByAny
            $ enemyIs Cards.hotelManager
         ]

instance RunMessage TheTrueCulpritV6 where
  runMessage msg a@(TheTrueCulpritV6 attrs) = runQueueT $ case msg of
    UseCardAbility _ (ProxySource _ (isSource attrs -> True)) 1 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 1) details
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ AdvanceAgendaBy (toId attrs) AgendaAdvancedWithOther
      pure a
    AdvanceAgendaBy aid AgendaAdvancedWithDoom | aid == toId attrs && onSide B attrs -> do
      push R2
      pure a
    AdvanceAgendaBy aid AgendaAdvancedWithOther | aid == toId attrs && onSide B attrs -> do
      push R1
      pure a
    _ -> TheTrueCulpritV6 <$> liftRunMessage msg attrs
