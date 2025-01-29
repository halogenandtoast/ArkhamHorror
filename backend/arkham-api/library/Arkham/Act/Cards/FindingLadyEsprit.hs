module Arkham.Act.Cards.FindingLadyEsprit (findingLadyEsprit) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Message.Lifted.Choose
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Maybe (fromJust)
import Data.Set qualified as Set

newtype FindingLadyEsprit = FindingLadyEsprit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findingLadyEsprit :: ActCard FindingLadyEsprit
findingLadyEsprit =
  act
    (1, A)
    FindingLadyEsprit
    Cards.findingLadyEsprit
    (Just $ GroupClueCost (PerPlayer 1) (withTrait Bayou))

instance RunMessage FindingLadyEsprit where
  runMessage msg a@(FindingLadyEsprit attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      ladyEsprit <- fetchCard Assets.ladyEsprit
      ladyEspritSpawnLocation <- fromJust . headMay <$> bayouLocations
      createAssetAt_ ladyEsprit (AtLocation ladyEspritSpawnLocation)

      setAsideLocations <- select $ SetAsideCardMatch LocationCard
      let
        traits =
          toList
            $ Set.unions (map toTraits setAsideLocations)
            `intersect` setFromList
              [NewOrleans, Riverside, Wilderness, Unhallowed]
        locationsFor t = filter (member t . toTraits) setAsideLocations
      setAsideLocationsWithLabels <-
        concatMapM
          (\t -> locationsWithLabels t (locationsFor t))
          traits
      for_ setAsideLocationsWithLabels \(label, card) -> do
        placeLocation card >>= (`setLocationLabel` label)
      doStep 2 msg
      pure a
    DoStep 2 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      lead <- getLead
      curseOfTheRougarouSet <-
        filter (not . (`cardMatch` Enemies.theRougarou))
          . map EncounterCard
          <$> gatherEncounterSet EncounterSet.CurseOfTheRougarou

      theRougarou <- genCard Enemies.theRougarou
      rougarouSpawnLocations <- nonBayouLocations
      chooseTargetM lead rougarouSpawnLocations (createEnemyAt_ theRougarou)

      shuffleEncounterDiscardBackIn
      shuffleCardsIntoDeck Deck.EncounterDeck curseOfTheRougarouSet

      curseOfTheRougarou <- genCard Treacheries.curseOfTheRougarou
      addCampaignCardToDeck lead DoNotShuffleIn curseOfTheRougarou
      push $ CreateWeaknessInThreatArea curseOfTheRougarou lead
      advanceActDeck attrs
      pure a
    _ -> FindingLadyEsprit <$> liftRunMessage msg attrs
