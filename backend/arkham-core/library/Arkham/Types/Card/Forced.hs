module Arkham.Types.Card.Forced where

import Arkham.Types.Card
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import ClassyPrelude

inHandAtEndOfRound :: MonadIO m => InvestigatorId -> Card -> m [Message]
inHandAtEndOfRound iid (PlayerCard MkPlayerCard {..}) = case pcCardCode of
  "01013" -> pure
    [ RevealInHand pcId
    , InvestigatorAssignDamage iid (PlayerCardSource pcId) 0 2
    ]
  _ -> pure []
inHandAtEndOfRound _ (EncounterCard _) = pure []
