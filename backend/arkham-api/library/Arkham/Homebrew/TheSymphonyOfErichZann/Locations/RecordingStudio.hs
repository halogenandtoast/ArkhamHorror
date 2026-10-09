module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.RecordingStudio (recordingStudio) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (scenarioI18n)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (ScenarioDiscard))
import Arkham.Trait (toTraits)

newtype RecordingStudio = RecordingStudio LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recordingStudio :: LocationCard RecordingStudio
recordingStudio =
  locationWith RecordingStudio Cards.recordingStudio 2 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) Anywhere

instance HasAbilities RecordingStudio where
  getAbilities (RecordingStudio a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "recordingStudio.reveal"
          $ restricted a 1 (exists InEncounterDiscard)
          $ forced
          $ RevealLocation #after Anyone (be a)
      , scenarioI18n
          $ withI18nTooltip "recordingStudio.searchDiscard"
          $ groupLimit PerRound
          $ restricted
            a
            2
            (Here <> exists (InEncounterDiscard <> basic (#treachery <> withTrait Music)))
            freeTrigger_
      ]

instance RunMessage RecordingStudio where
  runMessage msg l@(RecordingStudio attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discards <- scenarioField ScenarioDiscard
      for_ (lastMay discards) $ drawCard iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discards <- scenarioField ScenarioDiscard
      let music = [c | c <- discards, Music `member` toTraits (toCard c)]
      chooseOneM iid $ scenarioI18n $ scope "recordingStudio" do
        targets music $ drawCard iid
      pure l
    _ -> RecordingStudio <$> liftRunMessage msg attrs
