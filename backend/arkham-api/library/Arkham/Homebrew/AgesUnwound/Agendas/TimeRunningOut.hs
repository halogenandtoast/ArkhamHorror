module Arkham.Homebrew.AgesUnwound.Agendas.TimeRunningOut (timeRunningOut) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (AdditionalActions, AnySkillValue))
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (getStandardActions)
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy

newtype TimeRunningOut = TimeRunningOut AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeRunningOut :: AgendaCard TimeRunningOut
timeRunningOut = agenda (2, A) TimeRunningOut Cards.timeRunningOut (Static 6)

{- | "Forced - At the end of your turn, if you have any standard actions
remaining: You gain 1 action for the next round."

Plus the @[action]@ agenda 1b hands to whichever agenda is current.

"Any standard actions remaining" is checked in the handler, not as a criterion:
'Arkham.Matcher.InvestigatorWithActionsRemaining' reads only
@InvestigatorRemainingActions@ (@Game.hs:1342@), so it would miss an unspent Leo
De Luca action -- which is exactly the case this campaign's "standard action"
rule exists to cover.
-}
instance HasAbilities TimeRunningOut where
  getAbilities (TimeRunningOut a) =
    [ forcedAbility a 1 $ TurnEnds #when You
    , grantedAidFromAfarAbility a 2
    ]

instance RunMessage TimeRunningOut where
  runMessage msg a@(TimeRunningOut attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      standard <- getStandardActions iid
      when (standard > 0) do
        {- "for the next round": an 'AdditionalActions' modifier that survives
        into the next turn, rather than 'GainActions' -- @Do BeginRound@
        re-derives @remainingActions@ from scratch (@Investigator/Runner.hs:2182@)
        and there is no positive counterpart to 'FewerActions'.
        'EffectEndOfNextTurnWindow' is the one window that spans "from now until
        the end of your next turn": it advances to the turn window at
        @BeginTurn@ and is disabled at @EndTurn@ (@Effect/Runner.hs:66@).
        'nextTurnModifier' would be disabled *at* @BeginTurn@, before the action
        could be spent. -}
        effectWithSource (attrs.ability 1) iid do
          removeOn $ EffectEndOfNextTurnWindow iid
          apply $ AdditionalActions (ikey' "timeRunningOut.additionalAction") (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawAidFromAfar iid
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

      Agenda 1b's note applies: the mythos phase is after the round's actions
      have been derived, so these are plain gains and losses. -}
      if any (.face.isSymbol) tokens
        then do
          loseStandardActions iid (attrs.ability 1) 1
          roundModifier attrs iid (AnySkillValue 1)
        else do
          gainActions iid attrs 1
          roundModifier attrs iid (AnySkillValue (-1))
      push $ ResetChaosTokens (toSource attrs)
      pure a
    _ -> TimeRunningOut <$> liftRunMessage msg attrs
