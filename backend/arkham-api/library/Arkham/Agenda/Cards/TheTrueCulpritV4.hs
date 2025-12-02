module Arkham.Agenda.Cards.TheTrueCulpritV4 (theTrueCulpritV4) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Fight
import Arkham.Matcher

newtype TheTrueCulpritV4 = TheTrueCulpritV4 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV4 :: AgendaCard TheTrueCulpritV4
theTrueCulpritV4 = agenda (3, A) TheTrueCulpritV4 Cards.theTrueCulpritV4 (Static 14)

instance HasAbilities TheTrueCulpritV4 where
  getAbilities (TheTrueCulpritV4 attrs) =
    guard (onSide A attrs)
      *> [ controlled_ (proxied (assetIs Cards.tomeOfRituals) attrs) 1
             $ fightAction (AssetClueCost "Tome of Rituals" (assetIs Cards.tomeOfRituals) $ Static 2)
         , mkAbility attrs 2
             $ Objective
             $ forced
             $ EnemyDefeated #after Anyone ByAny
             $ enemyIs Cards.otherworldlyMeddler
         , restricted
             attrs
             2
             (exists $ enemyIs Cards.otherworldlyMeddler <> EnemyWithDoom (EqualTo $ Static 0))
             $ Objective
             $ forced AnyWindow
         ]

instance RunMessage TheTrueCulpritV4 where
  runMessage msg a@(TheTrueCulpritV4 attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      let source = toAbilitySource p 1
      sid <- getRandom
      aspect
        iid
        source
        (#willpower `InsteadOf` #combat)
        (setTarget attrs <$> mkChooseFight sid iid source)
      pure a
    Successful (Action.Fight, target) _ _ (isTarget attrs -> True) _ -> do
      removeDoom (attrs.ability 1) target 3
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      push $ if means == AgendaAdvancedWithDoom then R2 else R1
      pure a
    _ -> TheTrueCulpritV4 <$> liftRunMessage msg attrs
