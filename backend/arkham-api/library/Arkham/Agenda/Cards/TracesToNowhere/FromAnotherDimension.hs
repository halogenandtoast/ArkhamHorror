module Arkham.Agenda.Cards.TracesToNowhere.FromAnotherDimension (fromAnotherDimension) where

import Arkham.Agenda.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Treacheries

newtype FromAnotherDimension = FromAnotherDimension AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromAnotherDimension :: AgendaCard FromAnotherDimension
fromAnotherDimension = agenda (2, A) FromAnotherDimension Cards.fromAnotherDimension (Static 8)

instance HasAbilities FromAnotherDimension where
  getAbilities (FromAnotherDimension a) = [abductorDoomWatch a 1, abductInsteadOfDefeat a 2]

instance RunMessage FromAnotherDimension where
  runMessage msg a@(FromAnotherDimension attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      abductorDoomWatchRequest (attrs.ability 1)
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      abductorDoomWatchResolve (attrs.ability 1) iid tokens
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      abductInsteadOfDefeatEffect ws
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid ->
        fieldMap InvestigatorDeck (take 1 . map toCard . unDeck) iid >>= abduct
      for_ [Treacheries.theMysteryDeepens, Treacheries.backToSquareOne] \def ->
        push $ RemoveAllCopiesOfEncounterCardFromGame (cardIs def)
      shuffleEncounterDiscardBackIn
      n <- perPlayer 1
      cards <- take n . map toCard . unDeck <$> getEncounterDeck
      shuffleIntoLeadsDeck cards
      advanceAgendaDeck attrs
      pure a
    _ -> FromAnotherDimension <$> liftRunMessage msg attrs
