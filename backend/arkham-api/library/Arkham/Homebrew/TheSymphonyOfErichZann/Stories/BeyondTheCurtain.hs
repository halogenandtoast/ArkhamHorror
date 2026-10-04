module Arkham.Homebrew.TheSymphonyOfErichZann.Stories.BeyondTheCurtain (beyondTheCurtain) where

import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Key
import Arkham.Matcher
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
        selectEach (assetIs Assets.augusteGaudinMaestroOfSymphonies) \aid ->
          push $ RemoveFromGame (toTarget aid)
        -- "Then, spawn the set aside Auguste Gaudin (Conductor of the Void) at the Stage Hall."
        stageHall <- selectJust $ locationIs Locations.stageHall
        gaudin <- getSetAsideCard Enemies.augusteGaudinConductorOfTheVoid
        createEnemyAt_ gaudin stageHall

      {- "Flip this card over and attach it to the Auditorium." The back is a
      location of its own, so it is put into play rather than attached -- the
      engine has no Placeable instance for locations. -}
      void $ placeLocationCard Locations.theWindowToNothingness
      pure s
    _ -> BeyondTheCurtain <$> liftRunMessage msg attrs
