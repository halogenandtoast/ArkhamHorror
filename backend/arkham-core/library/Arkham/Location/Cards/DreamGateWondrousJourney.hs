module Arkham.Location.Cards.DreamGateWondrousJourney (
  dreamGateWondrousJourney,
  DreamGateWondrousJourney (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection

newtype DreamGateWondrousJourney = DreamGateWondrousJourney LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamGateWondrousJourney :: LocationCard DreamGateWondrousJourney
dreamGateWondrousJourney =
  locationWith DreamGateWondrousJourney Cards.dreamGateWondrousJourney 1 (Static 0)
    $ (connectedMatchersL .~ [RevealedLocation])
    . (revealedConnectedMatchersL .~ [RevealedLocation])

instance HasModifiersFor DreamGateWondrousJourney where
  getModifiersFor (LocationTarget lid) (DreamGateWondrousJourney a) | not (a `is` lid) = do
    pure $ toModifiers a [ConnectedToWhen RevealedLocation (LocationWithId $ toId a)]
  getModifiersFor target (DreamGateWondrousJourney a) | a `is` target = do
    pure $ toModifiers a [CannotBeEnteredBy AnyEnemy]
  getModifiersFor (InvestigatorTarget iid) (DreamGateWondrousJourney a) = do
    notLuke <- iid <!=~> investigatorIs Investigators.lukeRobinson
    pure $ toModifiers a [CannotEnter (toId a) | notLuke]
  getModifiersFor _ _ = pure []

instance HasAbilities DreamGateWondrousJourney where
  getAbilities (DreamGateWondrousJourney attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 (exists $ You <> investigatorIs Investigators.lukeRobinson)
            $ ForcedAbility
            $ PhaseEnds #when #investigation
        ]

instance RunMessage DreamGateWondrousJourney where
  runMessage msg l@(DreamGateWondrousJourney attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ RemoveLocation (toId attrs)
      here <- fieldMap InvestigatorLocation (== Just (toId attrs)) iid
      when here do
        revealedLocations <- selectList (RevealedLocation <> NotLocation (LocationWithId $ toId attrs))
        player <- getPlayer iid
        pushIfAny revealedLocations
          $ chooseOne player
          $ targetLabels revealedLocations (only . Move . move (toSource attrs) iid)

      pure l
    _ -> DreamGateWondrousJourney <$> runMessage msg attrs
