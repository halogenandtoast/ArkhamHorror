module Arkham.Homebrew.AgesUnwound.Agendas.TimeRunningOut (timeRunningOut) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (AdditionalActions, AnySkillValue))
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown.Helpers
import Arkham.I18n
import Arkham.Matcher

newtype TimeRunningOut = TimeRunningOut AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeRunningOut :: AgendaCard TimeRunningOut
timeRunningOut = agenda (2, A) TimeRunningOut Cards.timeRunningOut (Static 6)

{- | "__Forced__ - At the end of your turn, if you have any standard actions
remaining: You gain 1 action for the next round."

The criterion counts /standard/ actions, which is what the card says. The
agenda-back loss below ("Each investigator who reveals a symbol loses 1 action")
does not: losing takes remaining actions first and then any additional ones,
restricted or not (FAQ 1.10).
-}
instance HasAbilities TimeRunningOut where
  getAbilities (TimeRunningOut a) =
    [ restricted a 1 (youExist $ InvestigatorWithUnrestrictedActionsRemaining (atLeast 1))
        $ forced
        $ TurnEnds #when You
    , grantedAidFromAfarAbility a 2
    ]

instance RunMessage TimeRunningOut where
  runMessage msg a@(TimeRunningOut attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      effectWithSource (attrs.ability 1) iid do
        removeOn $ EffectEndOfNextTurnWindow iid
        apply $ AdditionalActions (ikey' "timeRunningOut.additionalAction") (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawAidFromAfar iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> requestChaosTokens iid attrs 1
      advanceAgendaDeck attrs
      pure a
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      {- Focused so the reveal lands in the drawn-token UI; the `continue_`
      below is what holds it there long enough to read, since focus and unfocus
      would otherwise resolve in the same tick. -}
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
      pure a
    _ -> TimeRunningOut <$> liftRunMessage msg attrs
