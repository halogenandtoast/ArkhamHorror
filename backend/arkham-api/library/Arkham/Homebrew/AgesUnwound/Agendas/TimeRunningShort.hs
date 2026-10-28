module Arkham.Homebrew.AgesUnwound.Agendas.TimeRunningShort (timeRunningShort) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (AdditionalActions, AnySkillValue))
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers
import Arkham.I18n
import Arkham.Matcher

newtype TimeRunningShort = TimeRunningShort AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeRunningShort :: AgendaCard TimeRunningShort
timeRunningShort = agenda (2, A) TimeRunningShort Cards.timeRunningShort (Static 6)

instance HasAbilities TimeRunningShort where
  getAbilities (TimeRunningShort a) =
    [ restricted a 1 (youExist $ InvestigatorWithUnrestrictedActionsRemaining (atLeast 1))
        $ forced
        $ TurnEnds #when You
    ]

instance RunMessage TimeRunningShort where
  runMessage msg a@(TimeRunningShort attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      effectWithSource (attrs.ability 1) iid do
        removeOn $ EffectEndOfNextTurnWindow iid
        apply $ AdditionalActions (ikey' "timeRunningShort.additionalAction") (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> requestChaosTokens iid attrs 1
      advanceAgendaDeck attrs
      pure a
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      continue_ iid
      if any (.face.isSymbol) tokens
        then do
          loseActions iid (attrs.ability 1) 1
          roundModifier attrs iid (AnySkillValue 1)
        else do
          gainActions iid attrs 1
          roundModifier attrs iid (AnySkillValue (-1))
      pure a
    _ -> TimeRunningShort <$> liftRunMessage msg attrs
