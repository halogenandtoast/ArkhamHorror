module Arkham.Act.Cards.FindingLadyEsprit
  ( FindingLadyEsprit(..)
  , findingLadyEsprit
  ) where

import Arkham.Prelude

import Arkham.Act.Attrs
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Data.HashSet qualified as HashSet
import Data.Maybe (fromJust)

newtype FindingLadyEsprit = FindingLadyEsprit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findingLadyEsprit :: ActCard FindingLadyEsprit
findingLadyEsprit = act
  (1, A)
  FindingLadyEsprit
  Cards.findingLadyEsprit
  (Just $ GroupClueCost (PerPlayer 1) (LocationWithTrait Bayou))

instance RunMessage FindingLadyEsprit where
  runMessage msg a@(FindingLadyEsprit attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
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
             [ TargetLabel (LocationTarget lid) [CreateEnemyAt theRougarou lid Nothing]
             | lid <- rougarouSpawnLocations
             ]
         ]
        <> [ ShuffleEncounterDiscardBackIn
           , ShuffleIntoEncounterDeck curseOfTheRougarouSet
           , AddCampaignCardToDeck
             leadInvestigatorId
             Treacheries.curseOfTheRougarou
           , CreateWeaknessInThreatArea curseOfTheRougarou leadInvestigatorId
           , AdvanceActDeck actDeckId (toSource attrs)
           ]
        )
    _ -> FindingLadyEsprit <$> runMessage msg attrs
