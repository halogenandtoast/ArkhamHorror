module Arkham.Types.Card.Forced where

import Arkham.Types.Card
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import ClassyPrelude

inHandAtEndOfRound :: MonadIO m => InvestigatorId -> Card -> m [Message]
inHandAtEndOfRound iid pc@(PlayerCard _) = case getCardCode pc of
  "01013" -> pure -- Dark Memory
    [ RevealInHand (getCardId pc)
    , InvestigatorAssignDamage iid (PlayerCardSource $ getCardId pc) 0 2
    ]
  _ -> pure []
inHandAtEndOfRound _ (EncounterCard _) = pure []
