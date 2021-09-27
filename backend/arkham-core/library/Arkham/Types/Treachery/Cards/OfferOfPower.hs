module Arkham.Types.Treachery.Cards.OfferOfPower where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype OfferOfPower = OfferOfPower TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

offerOfPower :: TreacheryCard OfferOfPower
offerOfPower = treachery OfferOfPower Cards.offerOfPower

instance TreacheryRunner env => RunMessage env OfferOfPower where
  runMessage msg t@(OfferOfPower attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push
        (chooseOne
          iid
          [ Label
            "Draw 2 cards and place 2 doom on agenda"
            [ DrawCards iid 2 False
            , PlaceDoomOnAgenda
            , PlaceDoomOnAgenda
            , AdvanceAgendaIfThresholdSatisfied
            ]
          , Label
            "Take 2 horror"
            [InvestigatorAssignDamage iid source DamageAny 0 2]
          ]
        )
    _ -> OfferOfPower <$> runMessage msg attrs
