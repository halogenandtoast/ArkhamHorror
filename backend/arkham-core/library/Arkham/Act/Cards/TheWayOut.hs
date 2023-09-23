module Arkham.Act.Cards.TheWayOut (
  TheWayOut (..),
  theWayOut,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype TheWayOut = TheWayOut ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWayOut :: ActCard TheWayOut
theWayOut = act (3, A) TheWayOut Cards.theWayOut Nothing

instance HasAbilities TheWayOut where
  getAbilities (TheWayOut a)
    | onSide A a =
        [ restrictedAbility
            a
            1
            (LocationExists $ locationIs Locations.theGateToHell)
            $ ForcedAbility
            $ RoundEnds Timing.When
        , restrictedAbility
            a
            2
            ( EachUndefeatedInvestigator
                $ InvestigatorAt
                $ locationIs
                  Locations.theGateToHell
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage TheWayOut where
  runMessage msg a@(TheWayOut attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      leadInvestigatorId <- getLeadInvestigatorId
      theGateToHell <- selectJust $ locationIs $ Locations.theGateToHell
      locations <-
        selectList
          $ FarthestLocationFromLocation theGateToHell Anywhere
      locationsWithInvestigatorsAndEnemiesAndConnectedLocations <-
        for locations $ \location -> do
          investigators <- selectList $ InvestigatorAt $ LocationWithId location
          enemies <- selectList $ EnemyAt $ LocationWithId location
          connectedLocations <-
            selectList
              $ AccessibleFrom
              $ LocationWithId
                location
          pure (location, investigators, enemies, connectedLocations)
      push
        $ chooseOrRunOne
          leadInvestigatorId
          [ targetLabel
            location
            [ chooseOrRunOne
                leadInvestigatorId
                [ targetLabel
                  connected
                  [ chooseOneAtATime leadInvestigatorId
                      $ [ targetLabel
                          investigator
                          [MoveTo $ move (toSource attrs) investigator connected]
                        | investigator <- investigators
                        ]
                      <> [ targetLabel enemy [EnemyMove enemy connected]
                         | enemy <- enemies
                         ]
                  ]
                | connected <- connectedLocations
                ]
            ]
          | (location, investigators, enemies, connectedLocations) <-
              locationsWithInvestigatorsAndEnemiesAndConnectedLocations
          ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    _ -> TheWayOut <$> runMessage msg attrs
