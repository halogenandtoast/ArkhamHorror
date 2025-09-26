module Arkham.Location.Cards.PhysicsClassroom (physicsClassroom) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose

newtype PhysicsClassroom = PhysicsClassroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicsClassroom :: LocationCard PhysicsClassroom
physicsClassroom = location PhysicsClassroom Cards.physicsClassroom 4 (PerPlayer 1)

instance HasAbilities PhysicsClassroom where
  getAbilities (PhysicsClassroom a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (CanDiscoverCluesAt $ RevealedLocation <> LocationWithAnyClues <> not_ (be a))
      $ freeReaction
      $ SkillTestResult #after You (WhileInvestigating $ be a)
      $ SuccessResult
      $ AtLeast (Static 2)

instance RunMessage PhysicsClassroom where
  runMessage msg l@(PhysicsClassroom attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      PhysicsClassroom <$> liftRunMessage msg (attrs & labelL .~ "physicsClassroom")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select
          $ locationWithDiscoverableCluesBy iid
          <> RevealedLocation
          <> LocationWithAnyClues
          <> not_ (be attrs)
      chooseOrRunOneM iid $ targets locations $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure l
    _ -> PhysicsClassroom <$> liftRunMessage msg attrs
