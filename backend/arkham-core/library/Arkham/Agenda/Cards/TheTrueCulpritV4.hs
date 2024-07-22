module Arkham.Agenda.Cards.TheTrueCulpritV4 (TheTrueCulpritV4 (..), theTrueCulpritV4) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Fight
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Prelude

newtype TheTrueCulpritV4 = TheTrueCulpritV4 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV4 :: AgendaCard TheTrueCulpritV4
theTrueCulpritV4 = agenda (3, A) TheTrueCulpritV4 Cards.theTrueCulpritV4 (Static 14)

instance HasAbilities TheTrueCulpritV4 where
  getAbilities (TheTrueCulpritV4 attrs) =
    guard (onSide A attrs)
      *> [ restrictedAbility
            (proxied (assetIs Cards.tomeOfRituals) attrs)
            1
            ControlsThis
            (fightAction $ AssetClueCost "Tome of Rituals" (assetIs Cards.tomeOfRituals) $ Static 2)
         , mkAbility attrs 2
            $ Objective
            $ ForcedAbility
            $ EnemyDefeated #after Anyone ByAny
            $ enemyIs Cards.otherworldlyMeddler
         , restrictedAbility
            attrs
            2
            (exists $ enemyIs Cards.otherworldlyMeddler <> EnemyWithDoom (EqualTo $ Static 0))
            $ Objective
            $ ForcedAbility AnyWindow
         ]

instance RunMessage TheTrueCulpritV4 where
  runMessage msg a@(TheTrueCulpritV4 attrs) =
    case msg of
      UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
        let source = toAbilitySource p 1
        sid <- getRandom
        chooseFight <-
          leftOr
            <$> aspect
              iid
              source
              (#willpower `InsteadOf` #combat)
              (setTarget attrs <$> mkChooseFight sid iid source)
        pushAll chooseFight
        pure a
      Successful (Action.Fight, target) _ _ (isTarget attrs -> True) _ -> do
        push $ RemoveDoom (toAbilitySource attrs 1) target 3
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
      _ -> TheTrueCulpritV4 <$> runMessage msg attrs
