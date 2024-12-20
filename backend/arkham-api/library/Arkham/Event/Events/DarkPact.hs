module Arkham.Event.Events.DarkPact (darkPact) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype DarkPact = DarkPact EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkPact :: EventCard DarkPact
darkPact = event DarkPact Cards.darkPact

instance HasAbilities DarkPact where
  getAbilities (DarkPact x) =
    [restricted x 1 InYourHand $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]]

instance RunMessage DarkPact where
  runMessage msg e@(DarkPact attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      iids <- select $ colocatedWith iid
      chooseTargetM iid iids \iid' -> assignDamage iid' attrs 2
      pure e
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      case toCard attrs of
        EncounterCard _ -> error "should be player card"
        VengeanceCard _ -> error "should be player card"
        PlayerCard pc -> do
          removeCardFromDeckForCampaign iid pc
          addCampaignCardToDeck iid =<< genPlayerCard Treacheries.thePriceOfFailure
          pure e
    _ -> DarkPact <$> liftRunMessage msg attrs
