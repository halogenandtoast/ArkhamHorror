module Arkham.Agenda.Cards.MadnessDrowns (madnessDrowns) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher

newtype MadnessDrowns = MadnessDrowns AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessDrowns :: AgendaCard MadnessDrowns
madnessDrowns = agenda (2, A) MadnessDrowns Cards.madnessDrowns (Static 7)

instance HasModifiersFor MadnessDrowns where
  getModifiersFor (MadnessDrowns a) = modifySelect a (EnemyWithTitle "Hastur") [EnemyFight 1]

instance HasAbilities MadnessDrowns where
  getAbilities (MadnessDrowns a)
    | onSide A a =
        [ restricted a 1 (exists $ EnemyWithTitle "Hastur" <> EnemyWithDamage (AtLeast $ PerPlayer 5))
            $ Objective
            $ forced AnyWindow
        ]
  getAbilities _ = []

instance RunMessage MadnessDrowns where
  runMessage msg a@(MadnessDrowns attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      palaceOfTheKing <- getJustLocationByName "Palace of the King"
      beastOfAldebaran <- getSetAsideCard Enemies.beastOfAldebaran
      createEnemyAt_ beastOfAldebaran palaceOfTheKing
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceAgenda attrs
      pure a
    _ -> MadnessDrowns <$> liftRunMessage msg attrs
