module Arkham.Homebrew.AgesUnwound.Agendas.Watched (watched) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Matcher

newtype Watched = Watched AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watched :: AgendaCard Watched
watched = agenda (2, A) Watched Cards.watched (Static 8)

instance HasModifiersFor Watched where
  getModifiersFor (Watched a) = modifySelf a [OtherDoomSubtracts]

instance HasAbilities Watched where
  getAbilities (Watched a) =
    [forcedAbility a 1 $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)]

instance RunMessage Watched where
  runMessage msg a@(Watched attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      step <- getCurrentActStep
      if step == 2
        then do
          putNewArkhamStreetsLocationIntoPlay >>= \case
            Just lid -> createSetAsideEnemy_ Enemies.eternitysSentinel_016 lid
            Nothing -> createSetAsideEnemy_ Enemies.eternitysSentinel_016 $ locationIs Locations.rivertown
        else do
          lead <- getLead
          createSetAsideEnemy_ Enemies.eternitysSentinel_017 $ locationWithInvestigator lead
      advanceAgendaDeck attrs
      pure a
    _ -> Watched <$> liftRunMessage msg attrs
