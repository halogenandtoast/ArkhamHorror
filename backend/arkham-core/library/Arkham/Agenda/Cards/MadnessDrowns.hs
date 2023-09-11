module Arkham.Agenda.Cards.MadnessDrowns (
  MadnessDrowns (..),
  madnessDrowns,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message

newtype MadnessDrowns = MadnessDrowns AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessDrowns :: AgendaCard MadnessDrowns
madnessDrowns = agenda (2, A) MadnessDrowns Cards.madnessDrowns (Static 7)

instance HasModifiersFor MadnessDrowns where
  getModifiersFor (EnemyTarget eid) (MadnessDrowns a) = do
    isHastur <- eid `isMatch` EnemyWithTitle "Hastur"
    pure $ toModifiers a [EnemyFight 1 | isHastur]
  getModifiersFor _ _ = pure []

instance HasAbilities MadnessDrowns where
  getAbilities (MadnessDrowns a)
    | onSide A a =
        [ restrictedAbility
            a
            1
            ( EnemyCriteria $
                EnemyExists
                  ( EnemyWithTitle "Hastur"
                      <> EnemyWithDamage (AtLeast $ PerPlayer 5)
                  )
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage MadnessDrowns where
  runMessage msg a@(MadnessDrowns attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      palaceOfTheKing <- getJustLocationByName "Palace of the King"
      beastOfAldebaran <- getSetAsideCard Enemies.beastOfAldebaran
      createBeastOfAldebaran <-
        createEnemyAt_
          beastOfAldebaran
          palaceOfTheKing
          Nothing
      pushAll
        [ createBeastOfAldebaran
        , ShuffleEncounterDiscardBackIn
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAgenda (toId attrs)
      pure a
    _ -> MadnessDrowns <$> runMessage msg attrs
