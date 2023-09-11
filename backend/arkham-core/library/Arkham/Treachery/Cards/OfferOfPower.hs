module Arkham.Treachery.Cards.OfferOfPower where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype OfferOfPower = OfferOfPower TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

offerOfPower :: TreacheryCard OfferOfPower
offerOfPower = treachery OfferOfPower Cards.offerOfPower

instance RunMessage OfferOfPower where
  runMessage msg t@(OfferOfPower attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      drawing <- drawCards iid attrs 2
      push
        $ chooseOne iid
        $ [ Label "Draw 2 cards and place 2 doom on agenda"
              $ [ drawing
                , PlaceDoomOnAgenda
                , PlaceDoomOnAgenda
                , AdvanceAgendaIfThresholdSatisfied
                ]
          , Label "Take 2 horror" [assignHorror iid attrs 2]
          ]
      pure t
    _ -> OfferOfPower <$> runMessage msg attrs
