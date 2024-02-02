module Arkham.Location.Cards.CosmicGate (
  cosmicGate,
  CosmicGate (..),
)
where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Timing qualified as Timing
import Arkham.Trait qualified as Trait

newtype CosmicGate = CosmicGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

cosmicGate :: LocationCard CosmicGate
cosmicGate =
  locationWith
    CosmicGate
    Cards.cosmicGate
    1
    (Static 1)
    (connectsToL .~ adjacentLocations)

instance HasAbilities CosmicGate where
  getAbilities (CosmicGate attrs) =
    withRevealedAbilities
      attrs
      [ cosmos attrs 1
      , forcedAbility attrs 2 $ Enters Timing.After You $ LocationWithId (toId attrs)
      , restrictedAbility
          attrs
          3
          ( Here
              <> LocationExists (LocationWithTrait Trait.Void <> NotLocation (LocationWithId $ toId attrs))
              <> InvestigatorExists (InvestigatorAt $ LocationWithId (toId attrs))
          )
          $ ActionAbility []
          $ ActionCost 1
          <> ScenarioResourceCost 1
      ]

-- withRevealedAbilities attrs []

instance RunMessage CosmicGate where
  runMessage msg l@(CosmicGate attrs) = case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      revealedLocations <- selectList RevealedLocation
      positions <- mapMaybeM findLocationInCosmos revealedLocations
      allEmpty <-
        concatMapM
          (\pos -> getEmptyPositionsInDirections pos [GridUp, GridDown, GridLeft, GridRight])
          positions

      if null allEmpty
        then cosmosFail attrs
        else do
          player <- getPlayer iid
          push
            $ chooseOrRunOne
              player
              [ GridLabel (cosmicLabel pos') (PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid) : msgs)
              | pos'@(Pos x y) <- allEmpty
              ]

      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      n <- getSpendableClueCount [iid]
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [Label "Spend 1 Clue" [SpendClues 1 [iid]] | n >= 1]
        <> [Label "Take 1 Horror" [assignHorror iid (toAbilitySource attrs 2) 1]]

      pure l
    UseCardAbility iid (isSource attrs -> True) 3 _ _ -> do
      investigators <- selectList $ investigatorAt (toId attrs)
      otherLocations <-
        selectList $ LocationWithTrait Trait.Void <> NotLocation (LocationWithId $ toId attrs)
      player <- getPlayer iid
      push
        $ chooseSome1
          player
          "Done moving investigators"
          [ targetLabel
            investigator
            [ chooseOne
                player
                [ targetLabel other [Move $ move (toAbilitySource attrs 3) investigator other]
                | other <- otherLocations
                ]
            ]
          | investigator <- investigators
          ]
      pure l
    _ -> CosmicGate <$> runMessage msg attrs
