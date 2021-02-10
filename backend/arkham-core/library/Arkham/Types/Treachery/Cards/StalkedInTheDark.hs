module Arkham.Types.Treachery.Cards.StalkedInTheDark
  ( stalkedInTheDark
  , StalkedInTheDark(..)
  )
where

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

newtype StalkedInTheDark = StalkedInTheDark TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedInTheDark :: TreacheryId -> a -> StalkedInTheDark
stalkedInTheDark uuid _ = StalkedInTheDark $ baseAttrs uuid "02143"

instance HasModifiersFor env StalkedInTheDark where
  getModifiersFor = noModifiersFor

instance HasActions env StalkedInTheDark where
  getActions i window (StalkedInTheDark attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env StalkedInTheDark where
  runMessage msg t@(StalkedInTheDark attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      mHuntingHorrorId <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorrorId of
        Just eid -> do
          lid <- getId @LocationId iid
          iids <- getSetList @InvestigatorId lid
          t <$ unshiftMessages
            ([Ready (EnemyTarget eid), EnemyEngageInvestigator eid iid]
            <> [ EnemyAttack iid' eid | iid' <- iids ]
            )
        Nothing ->
          t <$ unshiftMessages [Surge iid source, Discard $ toTarget attrs]
    _ -> StalkedInTheDark <$> runMessage msg attrs
