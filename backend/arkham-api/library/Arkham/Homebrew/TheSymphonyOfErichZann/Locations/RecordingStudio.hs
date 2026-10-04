module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.RecordingStudio (recordingStudio) where

import Arkham.Ability
import Arkham.Card
import Arkham.Trait (toTraits)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (scenarioI18n)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (ScenarioDiscard))

newtype RecordingStudio = RecordingStudio LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recordingStudio :: LocationCard RecordingStudio
recordingStudio = location RecordingStudio Cards.recordingStudio 2 (PerPlayer 1)

instance HasModifiersFor RecordingStudio where
  getModifiersFor (RecordingStudio a) = do
    -- "The door leading to this room is blocked. As an additional cost to move
    -- to Backstage Room, the investigators must spend 1 clue per investigator,
    -- as a group."
    modifySelfWhen a (not a.revealed) [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) Anywhere]

instance HasAbilities RecordingStudio where
  getAbilities (RecordingStudio a) =
    extend
      a
      [ -- "After you reveal Recording Studio: Draw the bottommost card of the encounter discard pile."
        mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , -- "[free]: Search the encounter discard pile for a Music treachery and draw it. (Group limit once per round)"
        groupLimit PerRound $ restricted a 2 Here $ freeReaction AnyWindow
      ]

instance RunMessage RecordingStudio where
  runMessage msg l@(RecordingStudio attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discards <- scenarioField ScenarioDiscard
      for_ (lastMay discards) \c -> drawCard iid (toCard c)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discards <- scenarioField ScenarioDiscard
      let music = [c | c <- discards, Music `member` toTraits (toCard c)]
      chooseOneM iid $ scenarioI18n $ scope "recordingStudio" do
        targets music \c -> drawCard iid (toCard c)
        labeled "noMusic" nothing
      pure l
    _ -> RecordingStudio <$> liftRunMessage msg attrs
