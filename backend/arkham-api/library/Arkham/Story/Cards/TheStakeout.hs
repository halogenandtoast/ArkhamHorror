module Arkham.Story.Cards.TheStakeout (theStakeout) where

import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Modifier (UIModifier (..))
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheStakeout = TheStakeout StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStakeout :: StoryCard TheStakeout
theStakeout = story TheStakeout Cards.theStakeout

instance HasModifiersFor TheStakeout where
  getModifiersFor (TheStakeout a) = do
    foundAVent <- remembered FoundAVent
    impersonatedAHighRoller <- remembered ImpersonatedAHighRoller
    convincedIsamaraToParticipateInTheHeist <- remembered ConvincedIsamaraToParticipateInTheHeist
    obtainedAnEmployeeUniform <- remembered ObtainedAnEmployeeUniform
    stoleAbarransKeys <- remembered StoleAbarransKeys
    cleanedOutTheHouse <- remembered CleanedOutTheHouse
    modifySelf a
      $ [UIModifier (OverlayCheckmark 12.0 31.4) | foundAVent]
      <> [UIModifier (OverlayCheckmark 12.0 35.5) | impersonatedAHighRoller]
      <> [UIModifier (OverlayCheckmark 12.0 43.2) | convincedIsamaraToParticipateInTheHeist]
      <> [UIModifier (OverlayCheckmark 12.0 50.8) | obtainedAnEmployeeUniform]
      <> [UIModifier (OverlayCheckmark 12.0 58.3) | stoleAbarransKeys]
      <> [UIModifier (OverlayCheckmark 12.0 62.5) | cleanedOutTheHouse]

instance RunMessage TheStakeout where
  runMessage msg s@(TheStakeout attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TheStakeout <$> liftRunMessage msg attrs
