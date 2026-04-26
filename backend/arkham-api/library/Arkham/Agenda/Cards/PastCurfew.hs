module Arkham.Agenda.Cards.PastCurfew (PastCurfew (..), pastCurfew) where

-- Constructor is only exported for testing purposes

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.SkillTest.Target (getSkillTestTarget)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PastCurfew = PastCurfew AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pastCurfew :: AgendaCard PastCurfew
pastCurfew = agenda (1, A) PastCurfew Cards.pastCurfew (Static 3)

instance HasAbilities PastCurfew where
  getAbilities (PastCurfew a) =
    [ skillTestAbility
        $ restricted a 1 (exists $ enemyIs Enemies.bystander <> at_ YourLocation) parleyAction_
    | onSide A a
    ]

instance RunMessage PastCurfew where
  runMessage msg a@(PastCurfew attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      bystanders <- select $ enemyIs Enemies.bystander <> enemyAtLocationWith iid
      chooseTargetM iid bystanders \enemy -> do
        sid <- getRandom
        parley sid iid (attrs.ability 1) enemy #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> toDiscardBy iid (attrs.ability 1) eid
        _ -> pure ()
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      advanceAgendaDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      pure a
    _ -> PastCurfew <$> liftRunMessage msg attrs
