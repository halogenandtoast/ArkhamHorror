module Arkham.Types.Act.Cards.FindingLadyEsprit
  ( FindingLadyEsprit(..)
  , findingLadyEsprit
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import Arkham.EncounterSet
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Treachery.Cards as Treacheries
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Data.Maybe (fromJust)

newtype FindingLadyEsprit = FindingLadyEsprit ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

findingLadyEsprit :: ActCard FindingLadyEsprit
findingLadyEsprit = act
  (1, A)
  FindingLadyEsprit
  Cards.findingLadyEsprit
  (Just $ GroupClueCost (PerPlayer 1) (Just $ LocationWithTrait Bayou))

bayouLocations
  :: (MonadReader env m, HasSet LocationId env [Trait])
  => m (HashSet LocationId)
bayouLocations = getSet [Bayou]

nonBayouLocations
  :: ( MonadReader env m
     , HasSet LocationId env ()
     , HasSet LocationId env [Trait]
     )
  => m (HashSet LocationId)
nonBayouLocations = difference <$> getLocationSet <*> bayouLocations

instance ActRunner env => RunMessage env FindingLadyEsprit where
  runMessage msg a@(FindingLadyEsprit attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      ladyEspritSpawnLocation <-
        fromJust . headMay . setToList <$> bayouLocations
      ladyEsprit <- PlayerCard <$> genPlayerCard Assets.ladyEsprit
      a <$ pushAll
        [ CreateStoryAssetAt ladyEsprit ladyEspritSpawnLocation
        , PutSetAsideIntoPlay (SetAsideLocationsTarget mempty)
        , NextAdvanceActStep aid 2
        ]
    NextAdvanceActStep aid 2 | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      curseOfTheRougarouSet <- gatherEncounterSet
        EncounterSet.CurseOfTheRougarou
      rougarouSpawnLocations <- setToList <$> nonBayouLocations
      theRougarou <- EncounterCard <$> genEncounterCard Enemies.theRougarou
      curseOfTheRougarou <- PlayerCard
        <$> genPlayerCard Treacheries.curseOfTheRougarou
      a <$ pushAll
        ([ chooseOne
             leadInvestigatorId
             [ CreateEnemyAt theRougarou lid Nothing
             | lid <- rougarouSpawnLocations
             ]
         ]
        <> [ ShuffleEncounterDiscardBackIn
           , ShuffleIntoEncounterDeck curseOfTheRougarouSet
           , AddCampaignCardToDeck
             leadInvestigatorId
             Treacheries.curseOfTheRougarou
           , CreateWeaknessInThreatArea curseOfTheRougarou leadInvestigatorId
           , NextAct aid "81006"
           ]
        )
    _ -> FindingLadyEsprit <$> runMessage msg attrs
