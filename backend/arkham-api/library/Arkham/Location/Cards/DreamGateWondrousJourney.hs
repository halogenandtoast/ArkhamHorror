module Arkham.Location.Cards.DreamGateWondrousJourney (dreamGateWondrousJourney) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

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
    modifySelect a (not_ $ LocationWithId a.id) [ConnectedToWhen RevealedLocation (be a)]
    modifySelf a [CannotBeEnteredBy AnyEnemy]
    modifySelect a (not_ $ investigatorIs Investigators.lukeRobinson) [CannotEnter (toId a)]
    modifySelect a AnyEnemy [CannotSpawnIn (be a)]

instance HasAbilities DreamGateWondrousJourney where
  getAbilities (DreamGateWondrousJourney a) =
    extendRevealed1 a
      $ restricted a 1 (youExist $ investigatorIs Investigators.lukeRobinson)
      $ forced
      $ PhaseEnds #when #investigation

instance RunMessage DreamGateWondrousJourney where
  runMessage msg l@(DreamGateWondrousJourney attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenAt iid attrs do
        revealedLocations <- getCanMoveToMatchingLocations iid (attrs.ability 1) RevealedLocation
        chooseTargetM iid revealedLocations $ moveTo (toSource attrs) iid
      removeLocation attrs
      pure l
    _ -> DreamGateWondrousJourney <$> liftRunMessage msg attrs
