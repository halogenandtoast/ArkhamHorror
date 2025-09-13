module Arkham.Agenda.Cards.ShowbusinessAsUsual (showbusinessAsUsual, showbusinessAsUsualEffect) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Act
import Arkham.Helpers.Investigator (getSkillValue)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype ShowbusinessAsUsual = ShowbusinessAsUsual AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

showbusinessAsUsual :: AgendaCard ShowbusinessAsUsual
showbusinessAsUsual = agenda (1, A) ShowbusinessAsUsual Cards.showbusinessAsUsual (Static 5)

instance RunMessage ShowbusinessAsUsual where
  runMessage msg a@(ShowbusinessAsUsual attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- getCurrentActStep
      if n == 1
        then do
          eachInvestigator \iid -> assignDamageAndHorror iid attrs 1 1
          cards <- shuffle =<< field LocationCardsUnderneath =<< selectJust (LocationWithTitle "Central Lot")
          for_ (nonEmpty cards) \(x :| xs) -> do
            setCardAside x
            for_ xs addToVictory
          push $ AdvanceToAct 1 Acts.andresRequest Act.B (toSource attrs)
        else do
          eachInvestigator \iid -> do
            sid <- getRandom
            choices <- mins <$> traverse (traverseToSnd (`getSkillValue` iid)) [minBound .. maxBound]
            chooseBeginSkillTest sid iid attrs iid choices (Fixed 2)
          remember TheInvestigatorsMadeTheirCallTime
      advanceAgendaDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      createCardEffect Cards.showbusinessAsUsual Nothing attrs iid
      loseResources iid attrs 2
      pure a
    _ -> ShowbusinessAsUsual <$> liftRunMessage msg attrs

newtype ShowbusinessAsUsualEffect = ShowbusinessAsUsualEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

showbusinessAsUsualEffect :: EffectArgs -> ShowbusinessAsUsualEffect
showbusinessAsUsualEffect = cardEffect ShowbusinessAsUsualEffect Cards.showbusinessAsUsual

instance RunMessage ShowbusinessAsUsualEffect where
  runMessage msg e@(ShowbusinessAsUsualEffect attrs) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      push $ LoseActions iid attrs.source 1
      disableReturn e
    _ -> ShowbusinessAsUsualEffect <$> liftRunMessage msg attrs
