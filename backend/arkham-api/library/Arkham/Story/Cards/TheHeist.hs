module Arkham.Story.Cards.TheHeist (theHeist) where

import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Modifier (UIModifier (..))
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted hiding (InvestigatorEliminated)

newtype TheHeist = TheHeist StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeist :: StoryCard TheHeist
theHeist = story TheHeist Cards.theHeist

instance HasModifiersFor TheHeist where
  getModifiersFor (TheHeist a) = do
    wonACultistMedallion <- remembered WonACultistMedallion
    observedTheStaff <- remembered ObservedTheStaff
    foundAbarransSigil <- remembered FoundAbarransSigil
    obtainedASchematic <- remembered ObtainedASchematic
    impersonatedAGuard <- remembered ImpersonatedAGuard
    stayedOutOfSight <- remembered StayedOutOfSight
    deliveredADecoyPackage <- remembered DeliveredADecoyPackage
    isamaraMesmerizedTheGuardsWithHerSong <- remembered IsamaraMesmerizedTheGuardsWithHerSong

    modifySelf a
      $ [UIModifier (OverlayCheckmark 10.8 31.8) | wonACultistMedallion]
      <> [UIModifier (OverlayCheckmark 10.8 36.1) | observedTheStaff]
      <> [UIModifier (OverlayCheckmark 10.8 40.3) | foundAbarransSigil]
      <> [UIModifier (OverlayCheckmark 10.8 44.5) | obtainedASchematic]
      <> [UIModifier (OverlayCheckmark 10.8 48.7) | impersonatedAGuard]
      <> [UIModifier (OverlayCheckmark 10.8 52.9) | stayedOutOfSight]
      <> [UIModifier (OverlayCheckmark 10.8 57.1) | deliveredADecoyPackage]
      <> [UIModifier (OverlayCheckmark 10.8 64.7) | isamaraMesmerizedTheGuardsWithHerSong]

instance RunMessage TheHeist where
  runMessage msg s@(TheHeist attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TheHeist <$> liftRunMessage msg attrs
