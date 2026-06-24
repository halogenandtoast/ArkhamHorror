module Arkham.Story.Cards.ToTheDreamlands (toTheDreamlands) where

import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ToTheDreamlands = ToTheDreamlands StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toTheDreamlands :: StoryCard ToTheDreamlands
toTheDreamlands = story ToTheDreamlands Cards.toTheDreamlands

instance RunMessage ToTheDreamlands where
  runMessage msg s@(ToTheDreamlands attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      for_
        [ Locations.theGreatAbyss
        , Locations.tunnelsUnderNgranek
        , Locations.stairwayToSarkomand
        , Locations.mistFilledCaverns
        ]
        \def -> whenJustM (getSetAsideCardMaybe def) \card -> do
          lid <- placeLocation card
          reveal lid
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      doStep 1 msg
      pure s
    DoStep 1 (ResolveThisStory iid (is attrs -> True)) -> do
      whenJustM (selectOne $ locationIs Locations.theGreatAbyss) $ moveTo attrs iid
      pure s
    _ -> ToTheDreamlands <$> liftRunMessage msg attrs
