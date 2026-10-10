module Arkham.Homebrew.AgesUnwound.Agendas.RageAgainstTheEnd (rageAgainstTheEnd) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (AdditionalActions))
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (getStandardActions)
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers
import Arkham.I18n
import Arkham.Matcher

newtype RageAgainstTheEnd = RageAgainstTheEnd AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rageAgainstTheEnd :: AgendaCard RageAgainstTheEnd
rageAgainstTheEnd = agenda (3, A) RageAgainstTheEnd Cards.rageAgainstTheEnd (Static 7)

{- | "Forced - When the doom threshold is checked, if there is 4 or more doom in
play: Each investigator loses 1 action. /
Forced - At the end of your turn, if you have any standard actions remaining: You
gain actions for the next round equal to the number you have remaining (to a
maximum of 3)."

@MythosStep AfterCheckDoomThreshold@ is the only timing point the engine offers
around the threshold check (@Game/Runner.hs:3176@ queues
@[AdvanceAgendaIfThresholdSatisfied, afterCheckDoomThreshold]@ as one step), so
ability 1 fires immediately after rather than immediately before it. The
difference is invisible: at 7 doom this agenda advances and defeats everyone
anyway, and nothing between the two points changes the doom count.

"4 or more doom in play" is total doom, not doom on this agenda.
-}
instance HasAbilities RageAgainstTheEnd where
  getAbilities (RageAgainstTheEnd a) =
    [ restricted a 1 (DoomCountIs $ atLeast 4) $ forced $ MythosStep AfterCheckDoomThreshold
    , forcedAbility a 2 $ TurnEnds #when You
    ]

instance RunMessage RageAgainstTheEnd where
  runMessage msg a@(RageAgainstTheEnd attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> loseStandardActions iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      standard <- getStandardActions iid
      let gained = min 3 standard
      when (gained > 0) do
        -- Same shape as agenda 2a's: see
        -- 'Arkham.Homebrew.AgesUnwound.Agendas.TimeRunningShort'.
        effectWithSource (attrs.ability 2) iid do
          removeOn $ EffectEndOfNextTurnWindow iid
          apply
            $ AdditionalActions (ikey' "rageAgainstTheEnd.additionalActions") (attrs.ability 2) gained
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- "Each remaining investigator is defeated."
      eachInvestigator (investigatorDefeated attrs)
      pure a
    _ -> RageAgainstTheEnd <$> liftRunMessage msg attrs
