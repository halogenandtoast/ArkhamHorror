module Arkham.Types.Act.Cards.FindingLadyEsprit
  ( FindingLadyEsprit(..)
  , findingLadyEsprit
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.EncounterSet qualified as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Trait
import Data.HashSet qualified as HashSet
import Data.Maybe (fromJust)

newtype FindingLadyEsprit = FindingLadyEsprit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findingLadyEsprit :: ActCard FindingLadyEsprit
findingLadyEsprit = act
  (1, A)
  FindingLadyEsprit
  Cards.findingLadyEsprit
  (Just $ GroupClueCost (PerPlayer 1) (LocationWithTrait Bayou))

instance ActRunner env => RunMessage env FindingLadyEsprit where
  runMessage msg a@(FindingLadyEsprit attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      ladyEspritSpawnLocation <-
        fromJust . headMay . setToList <$> bayouLocations
      ladyEsprit <- PlayerCard <$> genPlayerCard Assets.ladyEsprit
      setAsideLocations <- selectList $ SetAsideCardMatch $ CardWithType
        LocationType

      let
        traits =
          toList
            $ HashSet.unions (map toTraits setAsideLocations)
            `intersect` setFromList
                          [NewOrleans, Riverside, Wilderness, Unhallowed]
        locationsFor t = filter (member t . toTraits) setAsideLocations

      setAsideLocationsWithLabels <-
        concat
          <$> traverse (\t -> locationsWithLabels t (locationsFor t)) traits

      a <$ pushAll
        ([CreateStoryAssetAt ladyEsprit ladyEspritSpawnLocation]
        <> concat
             [ [ PlaceLocation card
               , SetLocationLabel (LocationId $ toCardId card) label
               ]
             | (label, card) <- setAsideLocationsWithLabels
             ]
        <> [NextAdvanceActStep aid 2]
        )
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
