module Arkham.Agenda.Cards.TheTrueCulpritV3 (theTrueCulpritV3) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Staff))

newtype TheTrueCulpritV3 = TheTrueCulpritV3 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV3 :: AgendaCard TheTrueCulpritV3
theTrueCulpritV3 = agenda (3, A) TheTrueCulpritV3 Cards.theTrueCulpritV3 (Static 6)

instance HasAbilities TheTrueCulpritV3 where
  getAbilities (TheTrueCulpritV3 attrs) =
    guard (onSide A attrs)
      *> [ doesNotProvokeAttacksOfOpportunity
             $ controlled
               (proxied (assetIs Cards.alienDevice) attrs)
               1
               (exists (EnemyAt YourLocation <> EnemyWithTrait Staff))
             $ actionAbilityWithCost
             $ ExhaustAssetCost (assetIs Cards.alienDevice)
             <> AssetClueCost "Alien Device" (assetIs Cards.alienDevice) (Static 2)
         , mkAbility attrs 2
             $ Objective
             $ forced
             $ EnemyDefeated #after Anyone ByAny
             $ enemyIs Cards.hotelManager
         ]

instance RunMessage TheTrueCulpritV3 where
  runMessage msg a@(TheTrueCulpritV3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (ProxySource originalSource (isSource attrs -> True)) 1 -> do
      staffWithIsElite <-
        select (EnemyWithTrait Staff <> enemyAtLocationWith iid)
          >>= traverse (traverseToSnd (<=~> EliteEnemy))
      leadChooseOrRunOneM do
        for_ staffWithIsElite \(staff, isElite) ->
          targeting staff do
            if isElite
              then nonAttackEnemyDamage (Just iid) originalSource 2 staff
              else addToVictory iid staff
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy aid means | aid == toId attrs && onSide B attrs -> do
      push $ if means == AgendaAdvancedWithDoom then R2 else R1
      pure a
    _ -> TheTrueCulpritV3 <$> liftRunMessage msg attrs
