{- | Agenda 1, On the Wings of the Wind.

The only agenda in the deck. It loops: every advance seeds another location with
doom, rides the Horseman out of Wiley's Swamp, and -- unless five locations are
doomed, which ends the scenario -- flips straight back to its "a" side.
-}
module Arkham.Homebrew.TheLegendOfSleepyHollow.Agendas.OnTheWingsOfTheWind (
  onTheWingsOfTheWind,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OnTheWingsOfTheWind = OnTheWingsOfTheWind AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Forced__ - When this agenda advances: Do not remove doom from each
location in play." Doom on locations is the scenario's clock, so the advance
sweep is taught to skip them.
-}
onTheWingsOfTheWind :: AgendaCard OnTheWingsOfTheWind
onTheWingsOfTheWind =
  agendaWith (1, A) OnTheWingsOfTheWind Cards.onTheWingsOfTheWind (Static 5)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomLocations = Nowhere})

-- | The ability is the printed __Forced__; the sweep above is what carries it out.
instance HasAbilities OnTheWingsOfTheWind where
  getAbilities (OnTheWingsOfTheWind a) =
    [ scenarioI18n
        $ withI18nTooltip "onTheWingsOfTheWind.doom"
        $ mkAbility a 1
        $ forced
        $ AgendaAdvances #when (AgendaWithId a.id)
    ]

instance RunMessage OnTheWingsOfTheWind where
  runMessage msg a@(OnTheWingsOfTheWind attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- "This effect cannot be canceled."
      candidates <- closestUndoomedToWileysSwamp
      unless (null candidates) do
        lead <- getLead
        chooseTargetM lead candidates \lid -> placeDoom attrs lid 1
      doStep 1 msg
      pure a
    DoStep 1 advance@(AdvanceAgenda (isSide B attrs -> True)) -> do
      swamp <- selectJust $ locationIs Locations.wileysSwamp
      selectOne headlessHorseman >>= \case
        Just horseman -> do
          -- "then he attacks each investigator at his location in player order"
          selectEach (InvestigatorAt $ locationWithEnemy horseman)
            $ initiateEnemyAttack horseman attrs
        Nothing ->
          selectOne (OutOfPlayEnemy SetAsideZone headlessHorseman)
            >>= traverse_ (push . EnemySpawnFromOutOfPlay SetAsideZone Nothing swamp)
      -- The original advance, not this step: 'doStep' wraps whatever it is given.
      doStep 2 advance
      pure a
    DoStep 2 (AdvanceAgenda (isSide B attrs -> True)) -> do
      doomed <- selectCount LocationWithAnyDoom
      if doomed >= 5 then push R1 else push $ RevertAgenda attrs.id
      pure a
    _ -> OnTheWingsOfTheWind <$> liftRunMessage msg attrs
