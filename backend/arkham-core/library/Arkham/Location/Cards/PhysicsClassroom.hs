module Arkham.Location.Cards.PhysicsClassroom (
  physicsClassroom,
  PhysicsClassroom (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype PhysicsClassroom = PhysicsClassroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicsClassroom :: LocationCard PhysicsClassroom
physicsClassroom =
  location PhysicsClassroom Cards.physicsClassroom 4 (PerPlayer 1)

instance HasAbilities PhysicsClassroom where
  getAbilities (PhysicsClassroom a) =
    withRevealedAbilities
      a
      [ limitedAbility (PlayerLimit PerRound 1) $
          restrictedAbility a 1 (CanDiscoverCluesAt $ RevealedLocation <> LocationWithAnyClues <> NotLocation (LocationWithId $ toId a)) $
            ReactionAbility
              ( SkillTestResult Timing.After You (WhileInvestigating $ LocationWithId $ toId a) $
                  SuccessResult $
                    AtLeast (Static 2)
              )
              Free
      ]

instance RunMessage PhysicsClassroom where
  runMessage msg l@(PhysicsClassroom attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      PhysicsClassroom <$> runMessage msg (attrs & labelL .~ "physicsClassroom")
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locations <- selectList $ locationWithDiscoverableCluesBy iid <> RevealedLocation <> LocationWithAnyClues <> NotLocation (LocationWithId $ toId attrs)
      push $ chooseOrRunOne iid [targetLabel lid [DiscoverCluesAtLocation iid lid 1 Nothing] | lid <- locations]
      pure l
    _ -> PhysicsClassroom <$> runMessage msg attrs
