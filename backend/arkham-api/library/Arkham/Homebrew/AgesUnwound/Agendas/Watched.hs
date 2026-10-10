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

{- | "Doom on cards other than this agenda subtracts from the total doom in play
instead of adding to it."
-}
instance HasModifiersFor Watched where
  getModifiersFor (Watched a) = modifySelf a [OtherDoomSubtracts]

instance HasAbilities Watched where
  getAbilities (Watched a) =
    [forcedAbility a 1 $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)]

instance RunMessage Watched where
  runMessage msg a@(Watched attrs) = runQueueT $ nightOfFireI18n $ case msg of
    -- "Forced - At the end of your turn, if you did not move at least once
    -- during your turn: Test [agility] (3). If you fail, take 1 damage."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      {- "If it is act 2, spawn the set-aside Eternity's Sentinel (Scourge in
      the Shadows) at a new Arkham Streets location. (If no such locations
      exist, spawn it at Rivertown instead.) Otherwise, spawn the set-aside
      Eternity's Sentinel (Watcher of the Ages) at the lead investigator's
      location."

      Act 2 is the only act this can fire on besides act 3 -- agenda 1b is what
      advances the act deck to 2a -- and by act 3 the Scourge has already been
      replaced by the Watcher, which is why the two branches name different
      copies. -}
      step <- getCurrentActStep
      if step == 2
        then
          putNewArkhamStreetsLocationIntoPlay >>= \case
            Just lid -> createSetAsideEnemy_ Enemies.eternitysSentinel_016 lid
            Nothing ->
              createSetAsideEnemy_ Enemies.eternitysSentinel_016
                $ locationIs Locations.rivertown
        else do
          lead <- getLead
          createSetAsideEnemy_ Enemies.eternitysSentinel_017
            $ locationWithInvestigator lead
      advanceAgendaDeck attrs
      pure a
    _ -> Watched <$> liftRunMessage msg attrs
