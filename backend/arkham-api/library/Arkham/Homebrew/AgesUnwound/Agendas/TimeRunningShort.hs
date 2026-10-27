module Arkham.Homebrew.AgesUnwound.Agendas.TimeRunningShort (timeRunningShort) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (AdditionalActions, AnySkillValue))
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy

newtype TimeRunningShort = TimeRunningShort AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeRunningShort :: AgendaCard TimeRunningShort
timeRunningShort = agenda (2, A) TimeRunningShort Cards.timeRunningShort (Static 6)

{- | "Forced - At the end of your turn, if you have any standard actions
remaining: You gain 1 action for the next round."

"Any standard actions remaining" gates the trigger, so the ability does not fire
at all on a turn that ends with none -- see
'Arkham.Homebrew.AgesUnwound.Agendas.TimeRunningOut' for why the plain
'Arkham.Matcher.InvestigatorWithActionsRemaining' will not do.
-}
instance HasAbilities TimeRunningShort where
  getAbilities (TimeRunningShort a) =
    [ restricted a 1 (youExist $ InvestigatorWithUnrestrictedActionsRemaining (atLeast 1))
        $ forced
        $ TurnEnds #when You
    ]

instance RunMessage TimeRunningShort where
  runMessage msg a@(TimeRunningShort attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      {- "for the next round": an 'AdditionalActions' modifier that survives into
      the next turn, rather than 'GainActions' -- @Do BeginRound@ re-derives
      @remainingActions@ from scratch and there is no positive counterpart to
      'FewerActions'. 'EffectEndOfNextTurnWindow' is the one window that spans
      "from now until the end of your next turn"; 'nextTurnModifier' would be
      disabled *at* @BeginTurn@, before the action could be spent. -}
      effectWithSource (attrs.ability 1) iid do
        removeOn $ EffectEndOfNextTurnWindow iid
        apply $ AdditionalActions (ikey' "timeRunningShort.additionalAction") (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- "Each investigator reveals a random token from the chaos bag."
      eachInvestigator \iid -> push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      advanceAgendaDeck attrs
      pure a
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      {- "Each investigator who reveals a symbol loses 1 action and gets +1 skill
      value during skill tests until the end of the round. /
      Each other investigator gains 1 action and gets -1 skill value during skill
      tests until the end of the round."

      The mythos phase is after the round's actions have been derived, so these
      are plain gains and losses. The token is focused and acknowledged so the
      reveal is actually visible -- see 'TimeRunningOut', which prompts but
      without the focus had nothing to show.

      "Lose N actions" is 'loseActions', not 'loseUnrestrictedActions': the campaign's
      /standard action/ term governs the effects that *count* actions, while losing
      follows the core rule -- remaining actions go first, and an investigator left
      with only additional actions loses those, whatever their limitation (FAQ 1.10). -}
      focusChaosTokens tokens \unfocus -> do
        continue_ iid
        push unfocus
        if any (.face.isSymbol) tokens
          then do
            loseActions iid (attrs.ability 1) 1
            roundModifier attrs iid (AnySkillValue 1)
          else do
            gainActions iid attrs 1
            roundModifier attrs iid (AnySkillValue (-1))
      push $ ResetChaosTokens (toSource attrs)
      pure a
    _ -> TimeRunningShort <$> liftRunMessage msg attrs
