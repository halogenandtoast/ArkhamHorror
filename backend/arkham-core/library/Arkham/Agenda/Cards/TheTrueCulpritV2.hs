module Arkham.Agenda.Cards.TheTrueCulpritV2 (
  TheTrueCulpritV2 (..),
  theTrueCulpritV2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Projection

newtype TheTrueCulpritV2 = TheTrueCulpritV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV2 :: AgendaCard TheTrueCulpritV2
theTrueCulpritV2 = agenda (3, A) TheTrueCulpritV2 Cards.theTrueCulpritV2 (Static 8)

instance HasAbilities TheTrueCulpritV2 where
  getAbilities (TheTrueCulpritV2 attrs) =
    guard (onSide A attrs)
      *> [ controlledAbility
            (proxied (assetIs Cards.sinisterSolution) attrs)
            1
            (exists $ You <> InvestigatorAt "Room 212")
            actionAbility
         , mkAbility attrs 2
            $ Objective
            $ ForcedAbility
            $ EnemyDefeated #after Anyone ByAny
            $ enemyIs Cards.otherworldlyMeddler
         ]

instance RunMessage TheTrueCulpritV2 where
  runMessage msg a@(TheTrueCulpritV2 attrs) =
    case msg of
      UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
        sid <- getRandom
        push $ beginSkillTest sid iid (toAbilitySource p 1) iid #intellect (Fixed 2)
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
      PassedThisSkillTest iid (AbilitySource (isProxySource attrs -> True) 1) -> do
        sinisterSolution <- selectJust $ assetIs Cards.sinisterSolution
        otherworldlyMeddler <- selectJust $ enemyIs Cards.otherworldlyMeddler
        clues <- field AssetClues sinisterSolution
        isReady <- otherworldlyMeddler <=~> ReadyEnemy

        player <- getPlayer iid
        pushWhen (clues >= 2)
          $ chooseOne player
          $ [ Label
              "remove 2 clues from Sinister Solution to either exhaust Otherworldly Meddler"
              [ RemoveClues (toSource sinisterSolution) (toTarget sinisterSolution) 2
              , Exhaust (toTarget otherworldlyMeddler)
              ]
            | isReady
            ]
          <> [ Label
                "remove 2 clues to deal it 2 damage"
                [ RemoveClues (toSource sinisterSolution) (toTarget sinisterSolution) 2
                , EnemyDamage otherworldlyMeddler $ nonAttack (toSource sinisterSolution) 2
                ]
             , Label "Skip" []
             ]

        pure a
      _ -> TheTrueCulpritV2 <$> runMessage msg attrs
