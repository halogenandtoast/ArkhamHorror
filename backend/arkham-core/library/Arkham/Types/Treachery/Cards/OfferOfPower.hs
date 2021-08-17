module Arkham.Types.Treachery.Cards.OfferOfPower where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs

newtype OfferOfPower = OfferOfPower TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

offerOfPower :: TreacheryCard OfferOfPower
offerOfPower = treachery OfferOfPower Cards.offerOfPower

instance HasModifiersFor env OfferOfPower

instance HasAbilities env OfferOfPower where
  getAbilities i window (OfferOfPower attrs) = getAbilities i window attrs

instance RunMessage env OfferOfPower where
  runMessage msg t@(OfferOfPower attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ pushAll
        [ chooseOne
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
        , Discard $ toTarget attrs
        ]
    _ -> OfferOfPower <$> runMessage msg attrs
