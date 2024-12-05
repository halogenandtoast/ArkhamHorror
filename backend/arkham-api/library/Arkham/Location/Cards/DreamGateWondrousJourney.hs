module Arkham.Location.Cards.DreamGateWondrousJourney (
  dreamGateWondrousJourney,
  DreamGateWondrousJourney (..),
)
where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection

newtype DreamGateWondrousJourney = DreamGateWondrousJourney LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamGateWondrousJourney :: LocationCard DreamGateWondrousJourney
dreamGateWondrousJourney =
  locationWith DreamGateWondrousJourney Cards.dreamGateWondrousJourney 1 (Static 0)
    $ (connectedMatchersL .~ [RevealedLocation])
    . (revealedConnectedMatchersL .~ [RevealedLocation])

instance HasModifiersFor DreamGateWondrousJourney where
  getModifiersFor (DreamGateWondrousJourney a) = do
    locations <-
      modifySelect
        a
        (not_ $ LocationWithId a.id)
        [ConnectedToWhen RevealedLocation (LocationWithId $ toId a)]
    self <- modifySelf a [CannotBeEnteredBy AnyEnemy]
    investigators <-
      modifySelect a (not_ $ investigatorIs Investigators.lukeRobinson) [CannotEnter (toId a)]
    enemies <- modifySelect a AnyEnemy [CannotSpawnIn (be a)]
    pure $ self <> investigators <> enemies <> locations

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
        revealedLocations <- getCanMoveToMatchingLocations iid (attrs.ability 1) RevealedLocation
        player <- getPlayer iid
        pushIfAny revealedLocations
          $ chooseOne player
          $ targetLabels revealedLocations (only . Move . move (toSource attrs) iid)

      pure l
    _ -> DreamGateWondrousJourney <$> runMessage msg attrs
