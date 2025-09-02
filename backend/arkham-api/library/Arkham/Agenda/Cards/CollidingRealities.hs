module Arkham.Agenda.Cards.CollidingRealities (collidingRealities) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck
import Arkham.Draw.Types
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Treacheries

newtype CollidingRealities = CollidingRealities AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collidingRealities :: AgendaCard CollidingRealities
collidingRealities = agenda (2, A) CollidingRealities Cards.collidingRealities (Static 13)

instance HasAbilities CollidingRealities where
  getAbilities (CollidingRealities a) =
    [ groupLimit PerRound
        $ mkAbility a 1
        $ forced
        $ ResolvesTreachery #after You (treacheryIs Treacheries.bleedingReality)
    ]

instance RunMessage CollidingRealities where
  runMessage msg a@(CollidingRealities attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCardsEdit iid (attrs.ability 1) 1 \d -> d {cardDrawDeck = ScenarioDeckByKey ReelDeck}
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> CollidingRealities <$> liftRunMessage msg attrs
