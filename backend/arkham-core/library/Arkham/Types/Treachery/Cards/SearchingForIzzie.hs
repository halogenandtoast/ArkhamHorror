module Arkham.Types.Treachery.Cards.SearchingForIzzie
  ( SearchingForIzzie(..)
  , searchingForIzzie
  ) where

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


import qualified Arkham.Types.Action as Action
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SearchingForIzzie = SearchingForIzzie TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForIzzie :: TreacheryId -> Maybe InvestigatorId -> SearchingForIzzie
searchingForIzzie uuid iid = SearchingForIzzie $ weaknessAttrs uuid iid "02011"

instance HasModifiersFor env SearchingForIzzie where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env SearchingForIzzie where
  getActions iid NonFast (SearchingForIzzie attrs@TreacheryAttrs {..}) = do
    investigatorLocationId <- getId @LocationId iid
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 2))
      | treacheryOnLocation investigatorLocationId attrs
      ]
  getActions _ _ _ = pure []

instance TreacheryRunner env => RunMessage env SearchingForIzzie where
  runMessage msg t@(SearchingForIzzie attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      farthestLocations <- map unFarthestLocationId <$> getSetList iid
      t <$ case farthestLocations of
        [lid] ->
          unshiftMessage (AttachTreachery treacheryId (LocationTarget lid))
        lids -> unshiftMessage
          (chooseOne
            iid
            [ AttachTreachery treacheryId (LocationTarget lid) | lid <- lids ]
          )
    UseCardAbility iid (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      withTreacheryLocation attrs $ \attachedLocationId -> do
        shroud <- unShroud <$> getCount attachedLocationId
        t <$ unshiftMessage
          (BeginSkillTest
            iid
            (TreacherySource treacheryId)
            (InvestigatorTarget iid)
            (Just Action.Investigate)
            SkillIntellect
            shroud
          )
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    _ -> SearchingForIzzie <$> runMessage msg attrs
