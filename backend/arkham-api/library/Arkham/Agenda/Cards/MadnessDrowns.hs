module Arkham.Agenda.Cards.MadnessDrowns (madnessDrowns) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Prelude

newtype MadnessDrowns = MadnessDrowns AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessDrowns :: AgendaCard MadnessDrowns
madnessDrowns = agenda (2, A) MadnessDrowns Cards.madnessDrowns (Static 7)

instance HasModifiersFor MadnessDrowns where
  getModifiersFor (MadnessDrowns a) = do
    modifySelect a (EnemyWithTitle "Hastur") [EnemyFight 1]

instance HasAbilities MadnessDrowns where
  getAbilities (MadnessDrowns a)
    | onSide A a =
        [ restricted
            a
            1
            (exists $ EnemyWithTitle "Hastur" <> EnemyWithDamage (AtLeast $ PerPlayer 5))
            $ Objective
            $ forced AnyWindow
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
