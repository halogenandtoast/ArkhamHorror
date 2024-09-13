module Arkham.Campaigns.TheInnsmouthConspiracy.Helpers where

import Arkham.Ability
import Arkham.Card.CardCode (HasCardCode)
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Effect.Types (makeEffectBuilder)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (CreateEffect))
import Arkham.Prelude
import Arkham.Source

needsAir :: (HasCardCode a, Sourceable a) => a -> Int -> Ability
needsAir a n = restrictedAbility a n (youExist $ at_ FullyFloodedLocation) $ forced $ TurnBegins #when You

struggleForAir :: (Sourceable a, HasQueue Message m) => a -> InvestigatorId -> m ()
struggleForAir a iid = push $ CreateEffect $ makeEffectBuilder "noair" Nothing a iid
