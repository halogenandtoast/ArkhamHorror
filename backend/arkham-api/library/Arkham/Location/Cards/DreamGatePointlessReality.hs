module Arkham.Location.Cards.DreamGatePointlessReality (dreamGatePointlessReality) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype DreamGatePointlessReality = DreamGatePointlessReality LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamGatePointlessReality :: LocationCard DreamGatePointlessReality
dreamGatePointlessReality = location DreamGatePointlessReality Cards.dreamGatePointlessReality 6 (Static 0)

instance HasModifiersFor DreamGatePointlessReality where
  getModifiersFor (DreamGatePointlessReality a) = do
    modifySelf a [CannotBeEnteredBy AnyEnemy]
    modifySelect a (not_ $ investigatorIs Investigators.lukeRobinson) [CannotEnter (toId a)]
    modifySelect a AnyEnemy [CannotSpawnIn (be a)]

instance HasAbilities DreamGatePointlessReality where
  getAbilities (DreamGatePointlessReality a) =
    extendRevealed
      a
      [ mkAbility a 1
          $ freeReaction
          $ SkillTestResult #after You (WhileInvestigating $ be a) #success
      , restricted a 2 (youExist $ investigatorIs Investigators.lukeRobinson)
          $ forced
          $ PhaseEnds #when #investigation
      ]

instance RunMessage DreamGatePointlessReality where
  runMessage msg l@(DreamGatePointlessReality attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      swapLocation attrs =<< genCard Locations.dreamGateWondrousJourney
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      whenAt iid attrs do
        revealedLocations <- getCanMoveToMatchingLocations iid (attrs.ability 1) RevealedLocation
        chooseTargetM iid revealedLocations \lid -> do
          moveTo (toSource attrs) iid lid
          assignHorror iid (toAbilitySource attrs 2) 2

      removeLocation attrs

      pure l
    _ -> DreamGatePointlessReality <$> liftRunMessage msg attrs
