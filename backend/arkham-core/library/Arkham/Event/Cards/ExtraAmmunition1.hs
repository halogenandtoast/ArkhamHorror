module Arkham.Event.Cards.ExtraAmmunition1 where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Trait

newtype ExtraAmmunition1 = ExtraAmmunition1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

extraAmmunition1 :: EventCard ExtraAmmunition1
extraAmmunition1 = event ExtraAmmunition1 Cards.extraAmmunition1

instance RunMessage ExtraAmmunition1 where
  runMessage msg e@(ExtraAmmunition1 attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      investigatorTargets <- guardAffectsColocated iid
      firearms <- selectList $ AssetWithTrait Firearm <> AssetControlledBy investigatorTargets
      player <- getPlayer iid
      push $ chooseOrRunOne player [targetLabel firearm [AddUses firearm Ammo 3] | firearm <- firearms]
      pure e
    _ -> ExtraAmmunition1 <$> runMessage msg attrs
