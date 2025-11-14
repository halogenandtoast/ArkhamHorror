module Arkham.Story.Cards.TheStakeout (theStakeout) where

import Arkham.Ability
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier (UIModifier (..))
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted hiding (InvestigatorEliminated)

newtype TheStakeout = TheStakeout StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStakeout :: StoryCard TheStakeout
theStakeout = story TheStakeout Cards.theStakeout

instance HasAbilities TheStakeout where
  getAbilities (TheStakeout a) =
    [mkAbility a 1 $ forced $ InvestigatorEliminated #when (You <> oneOf [InvestigatorWithAnyClues, InvestigatorWithResources (atLeast 1)])]

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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clueCount <- field InvestigatorClues iid
      resourceCount <- field InvestigatorResources iid
      discardAllClues (attrs.ability 1) iid
      placeTokens (attrs.ability 1) attrs #clue clueCount
      placeTokens (attrs.ability 1) attrs #resource resourceCount
      pure s
    _ -> TheStakeout <$> liftRunMessage msg attrs
