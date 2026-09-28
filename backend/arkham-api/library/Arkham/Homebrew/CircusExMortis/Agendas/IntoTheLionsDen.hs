module Arkham.Homebrew.CircusExMortis.Agendas.IntoTheLionsDen (
  intoTheLionsDen,
  heatOfTheMoment,
  heatOfTheMomentFailure,
) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Investigator (getHandCount)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Message.Lifted.Choose (chooseBeginSkillTest)
import Arkham.Projection

newtype IntoTheLionsDen = IntoTheLionsDen AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheLionsDen :: AgendaCard IntoTheLionsDen
intoTheLionsDen = agenda (1, A) IntoTheLionsDen Cards.intoTheLionsDen (Static 6)

{- | The back of agenda 1 ("Heat of the Moment") and of agenda 2 ("Popular Demand")
are word for word identical, so agenda 2 imports both halves from here.

X is one value for the whole round: the total ☾ sealed on every investigator's
card, read once before the first test.
-}
heatOfTheMoment :: (ReverseQueue m, Sourceable source) => source -> m ()
heatOfTheMoment source = do
  iids <- getInvestigators
  moons <- length <$> concatMapM getSealedMoonTokens iids
  for_ iids \iid -> do
    sid <- getRandom
    chooseBeginSkillTest sid iid source iid [#willpower, #agility] (Fixed moons)

{- | "must choose and discard half the cards in their hand and lose half the
resources in their resource pool (rounded up)"
-}
heatOfTheMomentFailure
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
heatOfTheMomentFailure source iid = do
  cards <- halfRoundedUp <$> getHandCount iid
  chooseAndDiscardCards iid source cards
  resources <- halfRoundedUp <$> field InvestigatorResources iid
  when (resources > 0) $ loseResources iid source resources
 where
  halfRoundedUp n = (n + 1) `div` 2

instance RunMessage IntoTheLionsDen where
  runMessage msg a@(IntoTheLionsDen attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      heatOfTheMoment attrs
      advanceAgendaDeckAfterSkillTest attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      heatOfTheMomentFailure attrs iid
      pure a
    _ -> IntoTheLionsDen <$> liftRunMessage msg attrs
