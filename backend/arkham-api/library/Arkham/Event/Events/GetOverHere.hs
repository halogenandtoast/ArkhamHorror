module Arkham.Event.Events.GetOverHere (getOverHere) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Location
import Arkham.Matcher

newtype GetOverHere = GetOverHere EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getOverHere :: EventCard GetOverHere
getOverHere = event GetOverHere Cards.getOverHere

instance RunMessage GetOverHere where
  runMessage msg e@(GetOverHere attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \lid -> do
        let m = LocationWithId lid
        enemies <-
          select
            $ NonEliteEnemy
            <> at_ (oneOf [m, ConnectedFrom m, LocationWithDistanceFrom 2 m Anywhere])
        sid <- getRandom
        chooseTargetM iid enemies \enemy -> do
          push $ EnemyEngageInvestigator enemy iid
          push $ FightEnemy enemy $ mkChooseFightPure sid iid (toSource attrs)
      pure e
    _ -> GetOverHere <$> liftRunMessage msg attrs
