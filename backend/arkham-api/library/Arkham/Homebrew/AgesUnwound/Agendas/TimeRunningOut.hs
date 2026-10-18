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

instance HasAbilities TimeRunningOut where
  getAbilities (TimeRunningOut a) =
    [ restricted a 1 (youExist $ InvestigatorWithStandardActionsRemaining (atLeast 1))
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
      continue_ iid
      if any (.face.isSymbol) tokens
        then do
          loseStandardActions iid (attrs.ability 1) 1
          roundModifier attrs iid (AnySkillValue 1)
        else do
          gainActions iid attrs 1
          roundModifier attrs iid (AnySkillValue (-1))
      pure a
    _ -> TimeRunningOut <$> liftRunMessage msg attrs
