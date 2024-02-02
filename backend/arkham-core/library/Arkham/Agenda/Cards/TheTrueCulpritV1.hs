module Arkham.Agenda.Cards.TheTrueCulpritV1 (
  TheTrueCulpritV1 (..),
  theTrueCulpritV1,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher

newtype TheTrueCulpritV1 = TheTrueCulpritV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theTrueCulpritV1 :: AgendaCard TheTrueCulpritV1
theTrueCulpritV1 = agenda (3, A) TheTrueCulpritV1 Cards.theTrueCulpritV1 (Static 6)

instance HasAbilities TheTrueCulpritV1 where
  getAbilities (TheTrueCulpritV1 attrs) =
    guard (onSide A attrs)
      *> [ controlledAbility
            (proxy (assetIs Cards.alienDevice) attrs)
            1
            (exists (enemyIs Cards.vengefulSpecter <> ExhaustedEnemy))
            (actionAbilityWithCost $ AssetClueCost "Alien Device" (assetIs Cards.alienDevice) $ Static 2)
         , mkAbility attrs 2
            $ Objective
            $ ForcedAbility
            $ EnemyDefeated #after Anyone ByAny
            $ enemyIs Cards.vengefulSpecter
         ]

instance RunMessage TheTrueCulpritV1 where
  runMessage msg a@(TheTrueCulpritV1 attrs) =
    case msg of
      UseThisAbility _ (ProxySource _ (isSource attrs -> True)) 1 -> do
        vengefulSpecter <- selectJust $ enemyIs Cards.vengefulSpecter
        push $ EnemyDamage vengefulSpecter $ nonAttack (toAbilitySource attrs 1) 2
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
      _ -> TheTrueCulpritV1 <$> runMessage msg attrs
