module Arkham.Types.Treachery.Cards.FinalRhapsody where

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

newtype FinalRhapsody = FinalRhapsody TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalRhapsody :: TreacheryId -> Maybe InvestigatorId -> FinalRhapsody
finalRhapsody uuid iid = FinalRhapsody $ weaknessAttrs uuid iid "02013"

instance HasModifiersFor env FinalRhapsody where
  getModifiersFor = noModifiersFor

instance HasActions env FinalRhapsody where
  getActions i window (FinalRhapsody attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env FinalRhapsody where
  runMessage msg t@(FinalRhapsody attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessage (RequestTokens source (Just iid) 5 SetAside)
    RequestedTokens source (Just iid) faces | isSource attrs source -> do
      let damageCount = count (`elem` [Skull, AutoFail]) faces
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid source DamageAny damageCount damageCount
        , ResetTokens source
        , Discard $ toTarget attrs
        ]
    _ -> FinalRhapsody <$> runMessage msg attrs
