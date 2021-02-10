module Arkham.Types.Treachery.Cards.MysteriousChanting where

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
 hiding (Cultist)

import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MysteriousChanting = MysteriousChanting TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousChanting :: TreacheryId -> a -> MysteriousChanting
mysteriousChanting uuid _ = MysteriousChanting $ baseAttrs uuid "01171"

instance HasModifiersFor env MysteriousChanting where
  getModifiersFor = noModifiersFor

instance HasActions env MysteriousChanting where
  getActions i window (MysteriousChanting attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env MysteriousChanting where
  runMessage msg t@(MysteriousChanting attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      enemies <- map unClosestEnemyId <$> getSetList (lid, [Cultist])
      case enemies of
        [] -> t <$ unshiftMessages
          [ FindAndDrawEncounterCard
            iid
            (EncounterCardMatchByType (EnemyType, Just Cultist))
          , Discard $ toTarget attrs
          ]
        xs -> t <$ unshiftMessages
          [ chooseOne iid [ PlaceDoom (EnemyTarget eid) 2 | eid <- xs ]
          , Discard $ toTarget attrs
          ]
    _ -> MysteriousChanting <$> runMessage msg attrs
