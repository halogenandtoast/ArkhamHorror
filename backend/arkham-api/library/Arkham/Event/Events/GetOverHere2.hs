module Arkham.Event.Events.GetOverHere2 (getOverHere2, GetOverHere2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.SkillType

newtype GetOverHere2 = GetOverHere2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getOverHere2 :: EventCard GetOverHere2
getOverHere2 =
  event GetOverHere2 Cards.getOverHere2

instance RunMessage GetOverHere2 where
  runMessage msg e@(GetOverHere2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- getJustLocation iid
      let m = LocationWithId lid
      enemies <-
        select
          $ NonEliteEnemy
          <> EnemyAt (LocationMatchAny [m, ConnectedFrom m, ConnectedFrom (ConnectedFrom m)])
      sid <- getRandom
      chooseTargetM iid enemies \enemy -> do
        push $ EnemyEngageInvestigator enemy iid
        push $ FightEnemy sid iid enemy (toSource attrs) Nothing SkillCombat False
      pure e
    _ -> GetOverHere2 <$> liftRunMessage msg attrs
