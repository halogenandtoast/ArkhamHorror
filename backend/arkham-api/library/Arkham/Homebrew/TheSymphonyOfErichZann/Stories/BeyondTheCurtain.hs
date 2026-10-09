module Arkham.Homebrew.TheSymphonyOfErichZann.Stories.BeyondTheCurtain (beyondTheCurtain) where

import Arkham.Helpers.Log (getHasRecord)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.TheSymphonyOfErichZann.Key
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype BeyondTheCurtain = BeyondTheCurtain StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheCurtain :: StoryCard BeyondTheCurtain
beyondTheCurtain = story BeyondTheCurtain Cards.beyondTheCurtain

instance RunMessage BeyondTheCurtain where
  runMessage msg s@(BeyondTheCurtain attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      {- "If the investigators have not reached act 3b..." -- act 3b is what
      records that all the musicians were saved, so that record is how we know. -}
      reachedAct3b <- getHasRecord YouSavedAllTheMusicians
      unless reachedAct3b do
        -- "If Auguste Gaudin (Maestro of Symphonies) is in play, remove him from the game."
        selectEach (assetIs Assets.augusteGaudinMaestroOfSymphonies)
          $ push
          . RemoveFromGame
          . toTarget
        {- "Then, spawn the set aside Auguste Gaudin (Conductor of the Void) enemy
        at the Stage Hall location."

        That sentence is written for the act 3 case, where act 2 has already put
        the Stage Hall into play and set Gaudin aside. But Coda Ultimatum is also
        reached by the agenda deck running out, which can happen while the
        investigators are still on act 1 or 2 -- and then there is no Stage Hall,
        nothing set aside, and on act 2 Gaudin is still in play with his own act
        asking you to defeat him.

        So: never a second copy of him, the Stage Hall when it exists and
        otherwise the Auditorium (his printed spawn, in play from setup), and
        `fetchCard` rather than `getSetAsideCard` because the card may be set
        aside, discarded or nowhere yet. -}
        gaudinInPlay <- selectAny $ enemyIs Enemies.augusteGaudinConductorOfTheVoid
        unless gaudinInPlay do
          mLocation <-
            (<|>)
              <$> selectOne (locationIs Locations.stageHall)
              <*> selectOne (locationIs Locations.auditorium)
          for_ mLocation \location -> do
            gaudin <- fetchCard Enemies.augusteGaudinConductorOfTheVoid
            createEnemyAt_ gaudin location

      -- "Flip this card over and attach it to the Auditorium."
      selectOne (locationIs Locations.auditorium) >>= traverse_ \auditorium -> do
        window <- fetchCard Treacheries.theWindowToNothingness
        createTreacheryAt_ window (AttachedToLocation auditorium)
      pure s
    _ -> BeyondTheCurtain <$> liftRunMessage msg attrs
