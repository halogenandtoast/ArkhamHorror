module Arkham.Agenda.Cards.TheFamiliar (theFamiliar) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act
import Arkham.Helpers.Card
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCard, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheFamiliar = TheFamiliar AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFamiliar :: AgendaCard TheFamiliar
theFamiliar = agenda (2, A) TheFamiliar Cards.theFamiliar (Static 6)

instance HasModifiersFor TheFamiliar where
  getModifiersFor (TheFamiliar a) = modifySelect a NonWeaknessEnemy [HealthModifier 2]

instance HasAbilities TheFamiliar where
  getAbilities (TheFamiliar a) =
    [ mkAbility a 1
        $ freeReaction
        $ EnemyDefeated #after You ByAny
        $ mapOneOf enemyIs [Enemies.nahab, Enemies.brownJenkin]
    ]

instance RunMessage TheFamiliar where
  runMessage msg a@(TheFamiliar attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      getCurrentActStep >>= \case
        1 -> do
          nahab <- getSetAsideCard Enemies.nahab
          walterGilmansRoom <- selectJust $ locationIs Locations.walterGilmansRoom
          createEnemyAt_ nahab walterGilmansRoom
        2 -> do
          nahab <- getSetAsideCard Enemies.nahab
          keziahsRoom <- selectJust $ locationIs Locations.keziahsRoom
          createEnemyAt_ nahab keziahsRoom
        3 -> do
          nahab <- getUniqueEnemy Enemies.nahab
          placeDoom attrs nahab 1
        _ -> error "Invalid act step"
      shuffleCardsIntoDeck Deck.EncounterDeck
        =<< getSetAsideCardsMatching (cardIs Treacheries.ghostlyPresence)
      push $ NextAdvanceAgendaStep attrs.id 1
      advanceAgendaDeck attrs
      pure a
    NextAdvanceAgendaStep aid 1 | aid == toId attrs -> do
      location <- selectJust $ LocationWithEnemy $ enemyIs Enemies.nahab
      selectOne (enemyIs Enemies.brownJenkin) >>= \case
        Just brownJenkin -> place brownJenkin (AtLocation location)
        Nothing -> findUniqueCard Enemies.brownJenkin >>= (`createEnemyAt_` location)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      push $ GainClues iid (attrs.ability 1) $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> TheFamiliar <$> liftRunMessage msg attrs
