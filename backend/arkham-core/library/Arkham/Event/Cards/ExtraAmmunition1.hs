module Arkham.Event.Cards.ExtraAmmunition1 where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait

newtype ExtraAmmunition1 = ExtraAmmunition1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraAmmunition1 :: EventCard ExtraAmmunition1
extraAmmunition1 = event ExtraAmmunition1 Cards.extraAmmunition1

instance RunMessage ExtraAmmunition1 where
  runMessage msg e@(ExtraAmmunition1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      firearms <-
        selectList $ AssetWithTrait Firearm <> AssetControlledBy
          (InvestigatorAt YourLocation)
      pushAll
        [ chooseOrRunOne
          iid
          [ targetLabel firearm [AddUses firearm Ammo 3] | firearm <- firearms ]
        , discard attrs
        ]
      pure e
    _ -> ExtraAmmunition1 <$> runMessage msg attrs
