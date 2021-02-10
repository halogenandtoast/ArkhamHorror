module Arkham.Types.Treachery.Cards.TwistOfFate
  ( TwistOfFate(..)
  , twistOfFate
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


import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TwistOfFate = TwistOfFate TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistOfFate :: TreacheryId -> a -> TwistOfFate
twistOfFate uuid _ = TwistOfFate $ baseAttrs uuid "02093"

instance HasModifiersFor env TwistOfFate where
  getModifiersFor = noModifiersFor

instance HasActions env TwistOfFate where
  getActions i window (TwistOfFate attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env TwistOfFate where
  runMessage msg t@(TwistOfFate attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (RequestTokens source (Just iid) 1 SetAside)
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
      let
        msgs = mapMaybe
          (\case
            ElderSign -> Nothing
            PlusOne -> Nothing
            Zero -> Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusOne ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusTwo ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusThree ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusFour ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusFive ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusSix ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusSeven ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            MinusEight ->
              Just (InvestigatorAssignDamage iid source DamageAny 1 0)
            Skull -> Just (InvestigatorAssignDamage iid source DamageAny 0 2)
            Cultist -> Just (InvestigatorAssignDamage iid source DamageAny 0 2)
            Tablet -> Just (InvestigatorAssignDamage iid source DamageAny 0 2)
            ElderThing ->
              Just (InvestigatorAssignDamage iid source DamageAny 0 2)
            AutoFail ->
              Just (InvestigatorAssignDamage iid source DamageAny 0 2)
          )
          tokens
      t <$ unshiftMessages
        (msgs <> [ResetTokens source, Discard $ toTarget attrs])
    _ -> TwistOfFate <$> runMessage msg attrs
