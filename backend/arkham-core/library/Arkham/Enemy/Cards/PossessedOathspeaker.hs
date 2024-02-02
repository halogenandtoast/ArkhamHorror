module Arkham.Enemy.Cards.PossessedOathspeaker (
  possessedOathspeaker,
  PossessedOathspeaker (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Agenda
import Arkham.Matcher

newtype PossessedOathspeaker = PossessedOathspeaker EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

possessedOathspeaker :: EnemyCard PossessedOathspeaker
possessedOathspeaker = enemy PossessedOathspeaker Cards.possessedOathspeaker (4, PerPlayer 5, 3) (2, 2)

instance HasModifiersFor PossessedOathspeaker where
  getModifiersFor (EnemyTarget eid) (PossessedOathspeaker attrs) | toId attrs == eid = do
    step <- getCurrentAgendaStep
    pure $ toModifiers attrs [CannotBeDamaged | step `elem` [1, 2]]
  getModifiersFor _ _ = pure []

instance HasAbilities PossessedOathspeaker where
  getAbilities (PossessedOathspeaker a) =
    withBaseAbilities a
      $ [ mkAbility a 1 $ ForcedAbility $ PhaseBegins #when #enemy
        , mkAbility a 2 $ ForcedAbility $ EnemyDefeated #when Anyone ByAny $ EnemyWithId $ toId a
        ]

instance RunMessage PossessedOathspeaker where
  runMessage msg e@(PossessedOathspeaker attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      if enemyExhausted attrs
        then
          push
            $ chooseOne lead
            $ [ Label "Ready Possessed Oathspeaker" [ready attrs]
              , Label "Place 1 doom on Possessed Oathspeaker" [placeDoom (toAbilitySource attrs 1) attrs 1]
              ]
        else push $ placeDoom (toAbilitySource attrs 1) attrs 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure e
    _ -> PossessedOathspeaker <$> runMessage msg attrs
