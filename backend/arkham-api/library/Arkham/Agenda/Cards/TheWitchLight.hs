module Arkham.Agenda.Cards.TheWitchLight (TheWitchLight (..), theWitchLight) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Act
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Placement

newtype TheWitchLight = TheWitchLight AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWitchLight :: AgendaCard TheWitchLight
theWitchLight = agenda (3, A) TheWitchLight Cards.theWitchLight (Static 8)

instance HasModifiersFor TheWitchLight where
  getModifiersFor (TheWitchLight a) =
    modifySelect a NonWeaknessEnemy [HealthModifier 3]

instance HasAbilities TheWitchLight where
  getAbilities (TheWitchLight a) =
    [ mkAbility a 1
        $ freeReaction
        $ EnemyDefeated #after You ByAny
        $ oneOf [enemyIs Enemies.nahab, enemyIs Enemies.brownJenkin]
    ]

instance RunMessage TheWitchLight where
  runMessage msg a@(TheWitchLight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      step <- getCurrentActStep
      mnahab <- getUniqueEnemyMaybe Enemies.nahab
      case step of
        3 -> do
          for_ mnahab \nahab -> placeDoom attrs nahab 1
          advanceAgendaDeck attrs
        _ -> do
          case mnahab of
            Nothing -> do
              mcard <- findCard (`cardMatch` cardIs Enemies.nahab)
              for_ mcard \card -> do
                obtainCard card
                setCardAside card
            Just nahab -> place nahab (OutOfPlay SetAsideZone)
          advanceAgendaDeck attrs
          placeDoomOnAgenda 4
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      push $ GainClues iid (attrs.ability 1) $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> TheWitchLight <$> liftRunMessage msg attrs
