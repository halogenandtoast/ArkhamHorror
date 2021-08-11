module Arkham.Types.Agenda.Cards.TheCurseSpreads
  ( TheCurseSpreads(..)
  , theCurseSpreads
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype TheCurseSpreads = TheCurseSpreads AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCurseSpreads :: AgendaCard TheCurseSpreads
theCurseSpreads =
  agenda (3, A) TheCurseSpreads Cards.theCurseSpreads (Static 8)

instance HasModifiersFor env TheCurseSpreads
instance HasActions TheCurseSpreads

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
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> TheCurseSpreads <$> runMessage msg attrs
