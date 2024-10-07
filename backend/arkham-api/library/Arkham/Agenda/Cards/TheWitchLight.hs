module Arkham.Agenda.Cards.TheWitchLight (TheWitchLight (..), theWitchLight) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Act
import Arkham.Helpers.Enemy
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype TheWitchLight = TheWitchLight AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWitchLight :: AgendaCard TheWitchLight
theWitchLight = agenda (3, A) TheWitchLight Cards.theWitchLight (Static 8)

instance HasModifiersFor TheWitchLight where
  getModifiersFor (EnemyTarget eid) (TheWitchLight a) = do
    isNonWeakness <- eid <=~> NonWeaknessEnemy
    pure $ toModifiers a [HealthModifier 3 | isNonWeakness]
  getModifiersFor _ _ = pure []

instance HasAbilities TheWitchLight where
  getAbilities (TheWitchLight a) =
    [ mkAbility a 1
        $ freeReaction
        $ EnemyDefeated #after You ByAny
        $ oneOf [enemyIs Enemies.nahab, enemyIs Enemies.brownJenkin]
    ]

instance RunMessage TheWitchLight where
  runMessage msg a@(TheWitchLight attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      nahab <- getUniqueEnemy Enemies.nahab
      step <- getCurrentActStep
      pushAll $ case step of
        3 ->
          [ PlaceDoom (toSource attrs) (toTarget nahab) 1
          , advanceAgendaDeck attrs
          ]
        _ ->
          [ PlaceEnemy nahab (OutOfPlay SetAsideZone)
          , advanceAgendaDeck attrs
          , PlaceDoomOnAgenda 4 CanNotAdvance
          ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      playerCount <- getPlayerCount
      push $ GainClues iid (attrs.ability 1) $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> TheWitchLight <$> runMessage msg attrs
