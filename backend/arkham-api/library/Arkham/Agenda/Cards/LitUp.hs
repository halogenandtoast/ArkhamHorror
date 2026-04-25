module Arkham.Agenda.Cards.LitUp (LitUp (..), litUp) where

-- Constructor is only exported for testing purposes

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.SkillTest.Target (getSkillTestTarget)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SpreadingFlames.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype LitUp = LitUp AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litUp :: AgendaCard LitUp
litUp = agenda (2, A) LitUp Cards.litUp (Static 5)

instance HasAbilities LitUp where
  getAbilities (LitUp a) =
    [ skillTestAbility
        $ restricted a 1 (exists $ enemyIs Enemies.bystander <> at_ YourLocation) parleyAction_
    | onSide A a
    ]

instance RunMessage LitUp where
  runMessage msg a@(LitUp attrs) = runQueueT $ case msg of
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
    AdvanceAgenda (isSide B attrs -> True) -> scenarioI18n do
      fires <- getSetAsideCardsMatching $ cardIs Treacheries.fire1
      addToEncounterDiscard $ take 4 fires
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #agility (Fixed 3)
      advanceAgendaDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      pure a
    _ -> LitUp <$> liftRunMessage msg attrs
