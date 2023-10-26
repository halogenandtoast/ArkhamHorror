module Arkham.Agenda.Cards.TheTrueCulpritV3 (
  TheTrueCulpritV3 (..),
  theTrueCulpritV3,
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
            $ controlledAbility
              (proxy (assetIs Cards.alienDevice) attrs)
              1
              (exists (EnemyAt YourLocation <> EnemyWithTrait Staff))
              ( actionAbilityWithCost
                  $ ExhaustAssetCost (assetIs Cards.alienDevice)
                  <> AssetClueCost "Alien Device" (assetIs Cards.alienDevice) (Static 2)
              )
         , mkAbility attrs 2
            $ Objective
            $ ForcedAbility
            $ EnemyDefeated #after Anyone ByAny
            $ enemyIs Cards.hotelManager
         ]

instance RunMessage TheTrueCulpritV3 where
  runMessage msg a@(TheTrueCulpritV3 attrs) =
    case msg of
      UseThisAbility iid (ProxySource originalSource (isSource attrs -> True)) 1 -> do
        staffWithIsElite <-
          selectList (EnemyWithTrait Staff <> enemyAtLocationWith iid)
            >>= traverse (traverseToSnd (<=~> EliteEnemy))
        player <- getPlayer iid
        push
          $ chooseOrRunOne
            player
            [ targetLabel
              staff
              [if isElite then AddToVictory (toTarget staff) else EnemyDamage staff $ nonAttack originalSource 2]
            | (staff, isElite) <- staffWithIsElite
            ]
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
      _ -> TheTrueCulpritV3 <$> runMessage msg attrs
