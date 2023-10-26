module Arkham.Agenda.Cards.TheTrueCulpritV7 (
  TheTrueCulpritV7 (..),
  theTrueCulpritV7,
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
import Arkham.Message qualified as Msg
import Arkham.Name
import Arkham.Trait (Trait (Cultist, Guest, Innocent))

newtype TheTrueCulpritV7 = TheTrueCulpritV7 AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV7 :: AgendaCard TheTrueCulpritV7
theTrueCulpritV7 = agenda (3, A) TheTrueCulpritV7 Cards.theTrueCulpritV7 (Static 8)

instance HasModifiersFor TheTrueCulpritV7 where
  getModifiersFor (EnemyTarget eid) (TheTrueCulpritV7 attrs) = do
    isGuest <- eid <=~> EnemyWithTrait Guest
    pure $ toModifiers attrs $ guard isGuest *> [LoseVictory, RemoveTrait Innocent, AddTrait Cultist]
  getModifiersFor _ _ = pure []

instance HasAbilities TheTrueCulpritV7 where
  getAbilities (TheTrueCulpritV7 attrs) =
    guard (onSide A attrs)
      *> ( [ controlledAbility
            (proxy (assetIs asset) attrs)
            1
            ( exists (enemyIs Cards.dimensionalShambler <> EnemyAt YourLocation <> CanEvadeEnemy (toSource attrs))
            )
            (evadeAction $ AssetClueCost (toTitle asset) (assetIs asset) $ Static 1)
           | asset <-
              [ Cards.alienDevice
              , Cards.managersKey
              , Cards.sinisterSolution
              , Cards.timeWornLocket
              , Cards.tomeOfRituals
              ]
           ]
            <> [ restrictedAbility
                  attrs
                  2
                  (Negate $ exists (EnemyWithTrait Cultist))
                  $ Objective
                  $ ForcedAbility AnyWindow
               ]
         )

instance RunMessage TheTrueCulpritV7 where
  runMessage msg a@(TheTrueCulpritV7 attrs) =
    case msg of
      UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
        dimensionalShambler <- selectJust $ enemyIs Cards.dimensionalShambler
        push $ Msg.EnemyEvaded iid dimensionalShambler
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
      _ -> TheTrueCulpritV7 <$> runMessage msg attrs
