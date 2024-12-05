module Arkham.Event.Events.TheStygianEye3 (theStygianEye3, TheStygianEye3 (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType

newtype TheStygianEye3 = TheStygianEye3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStygianEye3 :: EventCard TheStygianEye3
theStygianEye3 = event TheStygianEye3 Cards.theStygianEye3

instance HasModifiersFor TheStygianEye3 where
  getModifiersFor (TheStygianEye3 a) = do
    n <- selectCount $ ChaosTokenFaceIs #curse
    modified_ a (CardIdTarget $ toCardId a) [ReduceCostOf (CardWithId $ toCardId a) n]

instance RunMessage TheStygianEye3 where
  runMessage msg e@(TheStygianEye3 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      pushM $ roundModifiers attrs iid [SkillModifier sType 3 | sType <- allSkills]
      pure e
    _ -> TheStygianEye3 <$> runMessage msg attrs
