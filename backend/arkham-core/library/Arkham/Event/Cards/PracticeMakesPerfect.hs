module Arkham.Event.Cards.PracticeMakesPerfect where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (getIsCommittable)
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Practiced))

newtype PracticeMakesPerfect = PracticeMakesPerfect EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

practiceMakesPerfect :: EventCard PracticeMakesPerfect
practiceMakesPerfect = event PracticeMakesPerfect Cards.practiceMakesPerfect

instance RunMessage PracticeMakesPerfect where
  runMessage msg e@(PracticeMakesPerfect attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      search iid attrs iid [fromTopOfDeck 9] (#skill <> withTrait Practiced) (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      committable <- filterM (getIsCommittable iid) cards
      let
        choices =
          [ targetLabel
            card
            [ Msg.skillTestModifiers attrs card [IfSuccessfulModifier ReturnToHandAfterTest, MustBeCommitted]
            , SkillTestCommitCard iid card
            ]
          | card <- committable
          ]
      chooseOne iid $ if null choices then [Label "No cards found" []] else choices
      pure e
    _ -> PracticeMakesPerfect <$> lift (runMessage msg attrs)
