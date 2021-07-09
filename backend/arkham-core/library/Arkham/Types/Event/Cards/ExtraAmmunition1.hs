module Arkham.Types.Event.Cards.ExtraAmmunition1 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Control.Monad.Extra hiding (filterM)

newtype ExtraAmmunition1 = ExtraAmmunition1 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraAmmunition1 :: EventCard ExtraAmmunition1
extraAmmunition1 = event ExtraAmmunition1 Cards.extraAmmunition1

instance HasModifiersFor env ExtraAmmunition1 where
  getModifiersFor = noModifiersFor

instance HasActions env ExtraAmmunition1 where
  getActions i window (ExtraAmmunition1 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env ExtraAmmunition1 where
  runMessage msg e@(ExtraAmmunition1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      investigatorIds <- getSetList @InvestigatorId =<< getId @LocationId iid
      assetIds <- concatForM investigatorIds getSetList
      firearms <- filterM ((elem Firearm <$>) . getSetList) assetIds
      e <$ if null firearms
        then push . Discard $ toTarget attrs
        else pushAll
          [ chooseOne iid [ AddUses (AssetTarget aid) Ammo 3 | aid <- firearms ]
          , Discard (toTarget attrs)
          ]
    _ -> ExtraAmmunition1 <$> runMessage msg attrs
