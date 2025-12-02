module Arkham.Agenda.Cards.TheTrueCulpritV5 (theTrueCulpritV5) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types (Field (EnemyClues, EnemyForcedRemainingHealth))
import Arkham.Matcher
import Arkham.Projection

newtype TheTrueCulpritV5 = TheTrueCulpritV5 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV5 :: AgendaCard TheTrueCulpritV5
theTrueCulpritV5 = agenda (3, A) TheTrueCulpritV5 Cards.theTrueCulpritV5 (Static 6)

instance HasAbilities TheTrueCulpritV5 where
  getAbilities (TheTrueCulpritV5 attrs) =
    guard (onSide A attrs)
      *> [ doesNotProvokeAttacksOfOpportunity
             $ skillTestAbility
             $ controlled
               (proxied (assetIs Cards.sinisterSolution) attrs)
               1
               (exists $ EnemyAt YourLocation <> enemyIs Cards.vengefulSpecter)
               actionAbility
         , mkAbility attrs 2
             $ Objective
             $ forced
             $ EnemyDefeated #after Anyone ByAny
             $ enemyIs Cards.vengefulSpecter
         , restricted
             attrs
             2
             (exists $ enemyIs Cards.vengefulSpecter <> EnemyWithEqualFields EnemyClues EnemyForcedRemainingHealth)
             $ Objective
             $ forced AnyWindow
         ]

instance RunMessage TheTrueCulpritV5 where
  runMessage msg a@(TheTrueCulpritV5 attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (toAbilitySource p 1) iid #intellect (Fixed 1)
      pure a
    PassedThisSkillTestBy _ (isProxyAbilitySource attrs 1 -> True) n | n > 0 -> do
      sinisterSolution <- selectJust $ assetIs Cards.sinisterSolution
      vengefulSpecter <- selectJust $ enemyIs Cards.vengefulSpecter
      moveableClues <- min n <$> field AssetClues sinisterSolution
      moveTokens (attrs.ability 1) sinisterSolution vengefulSpecter #clue moveableClues
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      push $ if means == AgendaAdvancedWithDoom then R2 else R1
      pure a
    _ -> TheTrueCulpritV5 <$> liftRunMessage msg attrs
