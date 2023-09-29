module Arkham.Agenda.Cards.TheyreGettingOut (
  TheyreGettingOut (..),
  theyreGettingOut,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Act
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Trait

newtype TheyreGettingOut = TheyreGettingOut AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreGettingOut :: AgendaCard TheyreGettingOut
theyreGettingOut = agenda (3, A) TheyreGettingOut Cards.theyreGettingOut (Static 10)

instance HasAbilities TheyreGettingOut where
  getAbilities (TheyreGettingOut x) =
    [ forcedAbility x 1 $ PhaseEnds #when #enemy
    , forcedAbility x 2 (RoundEnds #when)
        `withCriteria` exists
          (UnengagedEnemy <> withTrait Ghoul <> NotEnemy (EnemyAt "Parlor"))
    ]

instance RunMessage TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      actSequence <- getCurrentActStep
      let resolution = if actSequence `elem` [1, 2] then Resolution 3 else NoResolution
      push $ ScenarioResolution resolution
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      enemiesToMove <- selectList $ UnengagedEnemy <> withTrait Ghoul <> NotEnemy (EnemyAt "Parlor")

      pushIfAny enemiesToMove
        $ chooseOneAtATime lead
        $ targetLabels enemiesToMove (\enemy -> only $ MoveToward (toTarget enemy) "Parlor")
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      ghoulCount <- selectCount $ withTrait Ghoul <> EnemyAt (LocationMatchAny ["Parlor", "Hallway"])
      pushAll $ replicate ghoulCount PlaceDoomOnAgenda
      pure a
    _ -> TheyreGettingOut <$> runMessage msg attrs
