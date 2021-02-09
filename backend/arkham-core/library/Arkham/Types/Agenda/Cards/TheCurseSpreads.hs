module Arkham.Types.Agenda.Cards.TheCurseSpreads
  ( TheCurseSpreads(..)
  , theCurseSpreads
  )
where


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
