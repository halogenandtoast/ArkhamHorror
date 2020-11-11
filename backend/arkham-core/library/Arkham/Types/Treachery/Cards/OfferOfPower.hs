{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.OfferOfPower where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype OfferOfPower = OfferOfPower Attrs
  deriving newtype (Show, ToJSON, FromJSON)

offerOfPower :: TreacheryId -> a -> OfferOfPower
offerOfPower uuid _ = OfferOfPower $ baseAttrs uuid "01178"

instance HasModifiersFor env OfferOfPower where
  getModifiersFor = noModifiersFor

instance HasActions env OfferOfPower where
  getActions i window (OfferOfPower attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env OfferOfPower where
  runMessage msg (OfferOfPower attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage
        (Ask iid $ ChooseOne
          [ Label
            "Draw 2 cards and place 2 doom on agenda"
            [ DrawCards iid 2 False
            , PlaceDoomOnAgenda
            , PlaceDoomOnAgenda
            , AdvanceAgendaIfThresholdSatisfied
            ]
          , Label "Take 2 horror" [InvestigatorAssignDamage iid source 0 2]
          ]
        )
      OfferOfPower <$> runMessage msg (attrs & resolved .~ True)
    _ -> OfferOfPower <$> runMessage msg attrs
