module Arkham.Types.Treachery.Cards.BeastOfTheBayou where

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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BeastOfTheBayou = BeastOfTheBayou TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beastOfTheBayou :: TreacheryId -> a -> BeastOfTheBayou
beastOfTheBayou uuid _ = BeastOfTheBayou $ baseAttrs uuid "81035"

instance HasModifiersFor env BeastOfTheBayou where
  getModifiersFor = noModifiersFor

instance HasActions env BeastOfTheBayou where
  getActions i window (BeastOfTheBayou attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BeastOfTheBayou where
  runMessage msg t@(BeastOfTheBayou attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      t <$ case mrougarou of
        Nothing ->
          unshiftMessages [PlaceDoomOnAgenda, Discard (toTarget attrs)]
        Just eid -> do
          locationId <- getId @LocationId eid
          connectedLocationIds <- map unConnectedLocationId
            <$> getSetList locationId
          investigatorIds <- concat <$> traverse
            (getSetList @InvestigatorId)
            (locationId : connectedLocationIds)
          case investigatorIds of
            [] -> unshiftMessages [PlaceDoomOnAgenda, Discard (toTarget attrs)]
            xs -> unshiftMessages
              ([ EnemyAttack iid' eid | iid' <- xs ]
              <> [Discard (toTarget attrs)]
              )
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAny
          n
          0
        )
    _ -> BeastOfTheBayou <$> runMessage msg attrs
