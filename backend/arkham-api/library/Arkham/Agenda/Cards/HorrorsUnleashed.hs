module Arkham.Agenda.Cards.HorrorsUnleashed (horrorsUnleashed) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier qualified as Modifier
import Arkham.Trait

newtype HorrorsUnleashed = HorrorsUnleashed AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorsUnleashed :: AgendaCard HorrorsUnleashed
horrorsUnleashed = agenda (3, A) HorrorsUnleashed Cards.horrorsUnleashed (Static 7)

instance HasAbilities HorrorsUnleashed where
  getAbilities (HorrorsUnleashed x) =
    [ restricted x 1 (exists $ InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth")
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance HasModifiersFor HorrorsUnleashed where
  getModifiersFor (HorrorsUnleashed attrs) =
    modifySelect attrs (EnemyWithTrait Abomination) [Modifier.EnemyFight 1, Modifier.EnemyEvade 1]

instance RunMessage HorrorsUnleashed where
  runMessage msg a@(HorrorsUnleashed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      broodOfYogSothoth <- select $ InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth"
      leadChooseOneAtATimeM $ targets broodOfYogSothoth \x -> push $ ChooseRandomLocation (toTarget x) mempty
      pure a
    ChosenRandomLocation target@(EnemyTarget _) lid -> do
      moveToward target lid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> HorrorsUnleashed <$> liftRunMessage msg attrs
