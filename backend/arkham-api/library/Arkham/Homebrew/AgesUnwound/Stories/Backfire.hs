module Arkham.Homebrew.AgesUnwound.Stories.Backfire (backfire) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Events
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message (ShuffleIn (ShuffleIn))
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted

newtype Backfire = Backfire StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Unstable Warding/ (@:ages-unwound:231b@), which flips it.
backfire :: StoryCard Backfire
backfire = story Backfire Cards.backfire

{- | "In your Campaign Log, record that /the investigators unleashed chaos./
Next to this, record the time. Shuffle each set-aside copy of Unleashed Chaos
into the encounter deck, along with the encounter discard pile. /
The investigator who flipped this card takes 1 physical trauma, and shuffles the
set-aside Unstable Energies into their deck, adding it to their deck for the
remainder of the campaign. /
Advance the act. Then, remove this card from the game."

The act is named by deck id. Scenario VI runs two act decks and three helpers
@error@ when it does (@Scenario.Types.scenarioActs@,
@Game.getRemainingActsMatching@, @Helpers.Act.getCurrentActStep@), so
@advanceCurrentAct@/@advanceTheAct@ are both unusable here -- deck 1 is the main
act deck in both scenarios that gather this card.
-}
instance RunMessage Backfire where
  runMessage msg s@(Backfire attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      record TheInvestigatorsUnleashedChaos
      recordTheTimeFor TheInvestigatorsUnleashedChaos

      shuffleSetAsideIntoEncounterDeck
        $ mapOneOf
          cardIs
          [ Treacheries.unleashedChaosIAccelerationI
          , Treacheries.unleashedChaosIProliferationI
          , Treacheries.unleashedChaosIMutationI
          ]
      shuffleEncounterDiscardBackIn

      push $ SufferTrauma iid 1 0
      addCampaignCardToDeck iid ShuffleIn Events.unstableEnergies

      selectOne (ActWithDeckId 1)
        >>= traverse_ \act -> push $ AdvanceAct act (toSource attrs) #other

      for_ (storyOtherSide attrs) removeFromGame
      removeFromGame attrs
      pure s
    _ -> Backfire <$> liftRunMessage msg attrs
