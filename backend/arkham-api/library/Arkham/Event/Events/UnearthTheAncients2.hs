module Arkham.Event.Events.UnearthTheAncients2 (unearthTheAncients2, UnearthTheAncients2 (..)) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest.Lifted (investigate_)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait

newtype Metadata = Metadata {chosenCards :: [CardId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UnearthTheAncients2 = UnearthTheAncients2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unearthTheAncients2 :: EventCard UnearthTheAncients2
unearthTheAncients2 = event (UnearthTheAncients2 . (`with` Metadata [])) Cards.unearthTheAncients2

-- Rules as written says that yes, you could commit the chosen cards to the
-- test, and put them into play when the test resolves. We may revisit this in
-- a future FAQ (not 2.0, which is about to be released).

instance RunMessage UnearthTheAncients2 where
  runMessage msg e@(UnearthTheAncients2 (attrs `With` metadata)) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <- select $ inHandOf NotForPlay iid <> basic (#seeker <> #asset)
      chooseUpToN iid 2 "Do not choose any more assets"
        $ [ targetLabel asset [HandleTargetChoice iid (toSource attrs) (toTarget asset)]
          | asset <- assets
          ]
      doStep 1 msg
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cid) -> do
      pure $ UnearthTheAncients2 $ attrs `with` Metadata (cid : chosenCards metadata)
    DoStep 1 (InvestigatorPlayEvent iid eid _ _ _) | eid == toId attrs -> do
      sid <- getRandom
      cards <- traverse getCard (chosenCards metadata)
      skillTestModifier sid attrs sid (SetDifficulty $ sum $ map getCost cards)
      investigate_ sid iid attrs
      pure e
    -- Unlike the level 0 printing this is not a replacement effect, so the location keeps
    -- its own clue discovery and putting the assets into play is its own ST.7 option.
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      cards <- traverse getCard (chosenCards metadata)
      skillTestCardOption attrs $ for_ cards \card -> do
        push $ Msg.putCardIntoPlay iid card
        when (Relic `member` toTraits card) $ drawCards iid attrs 1
      pure e
    _ -> UnearthTheAncients2 . (`with` metadata) <$> liftRunMessage msg attrs
