module Arkham.Agenda.Cards.TheTrueCulpritV5 (
  TheTrueCulpritV5 (..),
  theTrueCulpritV5,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
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
            $ controlledAbility
              (proxy (assetIs Cards.sinisterSolution) attrs)
              1
              (exists $ EnemyAt YourLocation <> enemyIs Cards.vengefulSpecter)
              actionAbility
         , mkAbility attrs 2
            $ Objective
            $ ForcedAbility
            $ EnemyDefeated #after Anyone ByAny
            $ enemyIs Cards.vengefulSpecter
         , restrictedAbility
            attrs
            2
            ( exists $ enemyIs Cards.vengefulSpecter <> EnemyWithEqualFields EnemyClues EnemyForcedRemainingHealth
            )
            $ Objective
            $ ForcedAbility AnyWindow
         ]

instance RunMessage TheTrueCulpritV5 where
  runMessage msg a@(TheTrueCulpritV5 attrs) =
    case msg of
      UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
        push $ beginSkillTest iid (toAbilitySource p 1) iid #intellect 1
        pure a
      PassedThisSkillTestBy _ (isProxyAbilitySource attrs 1 -> True) n | n > 0 -> do
        sinisterSolution <- selectJust $ assetIs Cards.sinisterSolution
        vengefulSpecter <- selectJust $ enemyIs Cards.vengefulSpecter
        moveableClues <- fieldMap AssetClues (min n) sinisterSolution
        pushWhen (moveableClues > 0)
          $ MovedClues (toSource sinisterSolution) (toTarget vengefulSpecter) moveableClues
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
      _ -> TheTrueCulpritV5 <$> runMessage msg attrs
