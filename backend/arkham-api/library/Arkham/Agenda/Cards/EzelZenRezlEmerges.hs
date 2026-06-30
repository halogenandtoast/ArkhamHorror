module Arkham.Agenda.Cards.EzelZenRezlEmerges (ezelZenRezlEmerges) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype EzelZenRezlEmerges = EzelZenRezlEmerges AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ezelZenRezlEmerges :: AgendaCard EzelZenRezlEmerges
ezelZenRezlEmerges = agenda (4, A) EzelZenRezlEmerges Cards.ezelZenRezlEmerges (Static 9)

instance HasModifiersFor EzelZenRezlEmerges where
  getModifiersFor (EzelZenRezlEmerges a) =
    modifySelect
      a
      (factionEnemy RedFaction)
      [RemoveKeyword Keyword.Aloof, RemoveKeyword #warring, AddKeyword Keyword.Hunter]

instance HasAbilities EzelZenRezlEmerges where
  getAbilities (EzelZenRezlEmerges a) =
    [mkAbility a 1 $ Objective $ forced $ IfEnemyDefeated_ #after (enemyIs Enemies.ezelZenRezl)]

instance RunMessage EzelZenRezlEmerges where
  runMessage msg a@(EzelZenRezlEmerges attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R2
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R5
      pure a
    _ -> EzelZenRezlEmerges <$> liftRunMessage msg attrs
