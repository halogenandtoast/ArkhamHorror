module Arkham.Types.Event.Cards.ExtraAmmunition1 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype ExtraAmmunition1 = ExtraAmmunition1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraAmmunition1 :: EventCard ExtraAmmunition1
extraAmmunition1 = event ExtraAmmunition1 Cards.extraAmmunition1

instance (EventRunner env) => RunMessage env ExtraAmmunition1 where
  runMessage msg e@(ExtraAmmunition1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      firearms <-
        selectListMap AssetTarget $ AssetWithTrait Firearm <> AssetOwnedBy
          (InvestigatorAt YourLocation)
      e <$ pushAll
        [ chooseOrRunOne iid [ AddUses firearm Ammo 3 | firearm <- firearms ]
        , discard attrs
        ]
    _ -> ExtraAmmunition1 <$> runMessage msg attrs
