module Arkham.Treachery.Cards.ThePriceOfFailure (
  thePriceOfFailure,
  ThePriceOfFailure (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Events
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ThePriceOfFailure = ThePriceOfFailure TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePriceOfFailure :: TreacheryCard ThePriceOfFailure
thePriceOfFailure = treachery ThePriceOfFailure Cards.thePriceOfFailure

instance RunMessage ThePriceOfFailure where
  runMessage msg t@(ThePriceOfFailure attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      case toCard attrs of
        VengeanceCard _ -> error "not a vengeance card"
        EncounterCard _ -> error "not an encounter card"
        PlayerCard pc -> do
          darkPact <- genPlayerCard Events.darkPact
          pushAll
            [ InvestigatorAssignDamage iid source DamageAny 2 2
            , PlaceDoomOnAgenda
            , AdvanceAgendaIfThresholdSatisfied
            , RemoveCardFromDeckForCampaign iid pc
            , AddCardToDeckForCampaign iid darkPact
            , RemoveTreachery (toId attrs)
            ]
          pure t
    _ -> ThePriceOfFailure <$> runMessage msg attrs
