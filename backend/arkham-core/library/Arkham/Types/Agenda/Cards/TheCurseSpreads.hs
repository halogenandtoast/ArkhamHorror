module Arkham.Types.Agenda.Cards.TheCurseSpreads
  ( TheCurseSpreads(..)
  , theCurseSpreads
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


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype TheCurseSpreads = TheCurseSpreads AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCurseSpreads :: TheCurseSpreads
theCurseSpreads = TheCurseSpreads
  $ baseAttrs "81004" "The Curse Spreads" (Agenda 3 A) (Static 8)

instance HasModifiersFor env TheCurseSpreads where
  getModifiersFor = noModifiersFor

instance HasActions env TheCurseSpreads where
  getActions i window (TheCurseSpreads x) = getActions i window x

getRougarou
  :: (MonadReader env m, HasId (Maybe StoryEnemyId) env CardCode)
  => m (Maybe EnemyId)
getRougarou = fmap unStoryEnemyId <$> getId (CardCode "81028")

instance AgendaRunner env => RunMessage env TheCurseSpreads where
  runMessage msg a@(TheCurseSpreads attrs@AgendaAttrs {..}) = case msg of
    EndInvestigation -> do
      mrougarou <- getRougarou
      case mrougarou of
        Nothing -> pure . TheCurseSpreads $ attrs & doomL +~ 1
        Just eid -> do
          notEngaged <- null <$> getSet @InvestigatorId eid
          pure . TheCurseSpreads $ if notEngaged
            then attrs & doomL +~ 1
            else attrs
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    _ -> TheCurseSpreads <$> runMessage msg attrs
