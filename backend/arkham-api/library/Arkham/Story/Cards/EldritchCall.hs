module Arkham.Story.Cards.EldritchCall (eldritchCall) where

import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Classes.HasQueue (removeAllMessagesMatching)
import Arkham.Helpers.Query (getLead)
import Arkham.Card
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EldritchCall = EldritchCall StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchCall :: StoryCard EldritchCall
eldritchCall = story EldritchCall Cards.eldritchCall

instance RunMessage EldritchCall where
  runMessage msg s@(EldritchCall attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      rage <- getCthulhuRage
      getCthulhuLocation >>= traverse_ \lid -> do
        investigators <- select $ investigatorAt lid
        if null investigators
          then
            -- "If there are no investigators at Cthulhu's location, discard this card
            -- and draw the top card of the Cthulhu deck." The scenario already queues
            -- the discard behind this resolution.
            drawCthulhuDeckCard iid attrs
          else
            {- "Otherwise, each investigator at Cthulhu's location tests [willpower]
            (X), where X is Cthulhu's Rage. If 1 or more investigators fail, discard
            cards from the top of the encounter deck until an enemy is discarded and
            spawn it at Cthulhu's location." -}
            for_ investigators \iid' -> do
              sid <- getRandom
              onFailedByEffect sid AnyValue attrs iid' $ doStep 1 msg
              beginSkillTest sid iid' attrs iid' #willpower (Fixed rage)
      pure s
    {- "If 1 or more investigators fail" — one spawn no matter how many fail, so the
    first copy of this step to run sweeps any siblings its fellow failures queued. -}
    DoStep 1 msg'@(ResolveThisStory _ (is attrs -> True)) -> do
      lift $ removeAllMessagesMatching (== DoStep 1 msg')
      getLead >>= \lead -> push $ DiscardUntilFirst lead (toSource attrs) Deck.EncounterDeck #enemy
      pure s
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      getCthulhuLocation >>= traverse_ (createEnemyAt_ (toCard card))
      pure s
    _ -> EldritchCall <$> liftRunMessage msg attrs
