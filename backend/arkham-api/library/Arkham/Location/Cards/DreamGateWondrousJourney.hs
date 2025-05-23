module Arkham.Location.Cards.DreamGateWondrousJourney (dreamGateWondrousJourney) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
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
    modifySelect
      a
      (not_ $ LocationWithId a.id)
      [ConnectedToWhen RevealedLocation (LocationWithId $ toId a)]
    modifySelf a [CannotBeEnteredBy AnyEnemy]
    modifySelect a (not_ $ investigatorIs Investigators.lukeRobinson) [CannotEnter (toId a)]
    modifySelect a AnyEnemy [CannotSpawnIn (be a)]

instance HasAbilities DreamGateWondrousJourney where
  getAbilities (DreamGateWondrousJourney attrs) =
    withRevealedAbilities
      attrs
      [ restricted attrs 1 (exists $ You <> investigatorIs Investigators.lukeRobinson)
          $ forced
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
