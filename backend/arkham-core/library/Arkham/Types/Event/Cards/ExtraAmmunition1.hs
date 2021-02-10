module Arkham.Types.Event.Cards.ExtraAmmunition1 where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Asset.Uses
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Trait
import Control.Monad.Extra hiding (filterM)

newtype ExtraAmmunition1 = ExtraAmmunition1 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraAmmunition1 :: InvestigatorId -> EventId -> ExtraAmmunition1
extraAmmunition1 iid uuid = ExtraAmmunition1 $ baseAttrs iid uuid "01026"

instance HasModifiersFor env ExtraAmmunition1 where
  getModifiersFor = noModifiersFor

instance HasActions env ExtraAmmunition1 where
  getActions i window (ExtraAmmunition1 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env ExtraAmmunition1 where
  runMessage msg e@(ExtraAmmunition1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      investigatorIds <- getSetList @InvestigatorId =<< getId @LocationId iid
      assetIds <- concatForM investigatorIds getSetList
      firearms <- filterM ((elem Firearm <$>) . getSetList) assetIds
      e <$ if null firearms
        then unshiftMessage . Discard $ toTarget attrs
        else unshiftMessages
          [ chooseOne iid [ AddUses (AssetTarget aid) Ammo 3 | aid <- firearms ]
          , Discard (toTarget attrs)
          ]
    _ -> ExtraAmmunition1 <$> runMessage msg attrs
