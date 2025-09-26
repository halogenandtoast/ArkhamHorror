module Arkham.Agenda.Cards.MarkedForSacrifice (markedForSacrifice) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher

newtype MarkedForSacrifice = MarkedForSacrifice AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedForSacrifice :: AgendaCard MarkedForSacrifice
markedForSacrifice = agenda (4, A) MarkedForSacrifice Cards.markedForSacrifice (Static 8)

instance HasModifiersFor MarkedForSacrifice where
  getModifiersFor (MarkedForSacrifice a) = modifySelect a NonWeaknessEnemy [HealthModifier 4]

instance HasAbilities MarkedForSacrifice where
  getAbilities (MarkedForSacrifice a) =
    [ mkAbility a 1
        $ freeReaction
        $ EnemyDefeated #after You ByAny
        $ mapOneOf enemyIs [Enemies.nahab, Enemies.brownJenkin]
    ]

instance RunMessage MarkedForSacrifice where
  runMessage msg a@(MarkedForSacrifice attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      gainClues iid (attrs.ability 1) $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> MarkedForSacrifice <$> liftRunMessage msg attrs
