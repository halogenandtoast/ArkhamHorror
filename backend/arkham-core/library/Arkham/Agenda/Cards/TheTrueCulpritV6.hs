module Arkham.Agenda.Cards.TheTrueCulpritV6 (
  TheTrueCulpritV6 (..),
  theTrueCulpritV6,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher

newtype TheTrueCulpritV6 = TheTrueCulpritV6 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV6 :: AgendaCard TheTrueCulpritV6
theTrueCulpritV6 = agenda (3, A) TheTrueCulpritV6 Cards.theTrueCulpritV6 (Static 6)

instance HasAbilities TheTrueCulpritV6 where
  getAbilities (TheTrueCulpritV6 attrs) =
    guard (onSide A attrs)
      *> [ restrictedAbility
            (proxy (assetIs Cards.timeWornLocket) attrs)
            1
            ControlsThis
            $ ReactionAbility
              (EnemyWouldAttack #when (InvestigatorAt YourLocation) AnyEnemyAttack $ enemyIs Cards.hotelManager)
              (AssetClueCost "Time-worn Locket" (assetIs Cards.timeWornLocket) $ Static 2)
         , mkAbility attrs 2
            $ Objective
            $ ForcedAbility
            $ EnemyDefeated #after Anyone ByAny
            $ enemyIs Cards.hotelManager
         ]

instance RunMessage TheTrueCulpritV6 where
  runMessage msg a@(TheTrueCulpritV6 attrs) =
    case msg of
      UseThisAbility _ (ProxySource _ (isSource attrs -> True)) 1 -> do
        push $ CancelNext (toSource attrs) AttackMessage
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
      _ -> TheTrueCulpritV6 <$> runMessage msg attrs
