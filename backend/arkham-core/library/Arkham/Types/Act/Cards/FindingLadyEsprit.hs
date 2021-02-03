module Arkham.Types.Act.Cards.FindingLadyEsprit
  ( FindingLadyEsprit(..)
  , findingLadyEsprit
  )
where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Trait

newtype FindingLadyEsprit = FindingLadyEsprit ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findingLadyEsprit :: FindingLadyEsprit
findingLadyEsprit =
  FindingLadyEsprit $ baseAttrs "81005" "Finding Lady Esprit" (Act 1 A) Nothing

instance ActionRunner env => HasActions env FindingLadyEsprit where
  getActions _ FastPlayerWindow (FindingLadyEsprit attrs@ActAttrs {..}) = do
    investigatorIds <- investigatorsInABayouLocation
    requiredClueCount <- getPlayerCountValue (PerPlayer 1)
    canAdvance' <- (>= requiredClueCount)
      <$> getSpendableClueCount investigatorIds
    pure [ AdvanceAct actId (toSource attrs) | canAdvance' ]
  getActions i window (FindingLadyEsprit x) = getActions i window x

investigatorsInABayouLocation
  :: ( MonadReader env m
     , HasSet LocationId env [Trait]
     , HasSet InvestigatorId env (HashSet LocationId)
     )
  => m [InvestigatorId]
investigatorsInABayouLocation = bayouLocations >>= getSetList

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
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      investigatorIds <- investigatorsInABayouLocation
      requiredClueCount <- getPlayerCountValue (PerPlayer 1)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ chooseOne iid [AdvanceAct aid (toSource attrs)]
          | iid <- investigatorIds
          ]
        )
      pure $ FindingLadyEsprit $ attrs & (sequenceL .~ Act 1 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      [ladyEspritSpawnLocation] <- setToList <$> bayouLocations
      a <$ unshiftMessages
        [ CreateStoryAssetAt "81019" ladyEspritSpawnLocation
        , PutSetAsideIntoPlay (SetAsideLocationsTarget mempty)
        , NextAdvanceActStep aid 2
        ]
    NextAdvanceActStep aid 2 | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      curseOfTheRougarouSet <- gatherEncounterSet
        EncounterSet.CurseOfTheRougarou
      rougarouSpawnLocations <- setToList <$> nonBayouLocations
      a <$ unshiftMessages
        ([ chooseOne
             leadInvestigatorId
             [ CreateEnemyAt "81028" lid | lid <- rougarouSpawnLocations ]
         ]
        <> [ ShuffleEncounterDiscardBackIn
           , ShuffleIntoEncounterDeck curseOfTheRougarouSet
           , AddCampaignCardToDeck leadInvestigatorId "81029"
           , CreateWeaknessInThreatArea "81029" leadInvestigatorId
           , NextAct aid "81006"
           ]
        )
    _ -> FindingLadyEsprit <$> runMessage msg attrs
