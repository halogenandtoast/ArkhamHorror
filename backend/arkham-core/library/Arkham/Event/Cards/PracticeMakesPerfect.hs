module Arkham.Event.Cards.PracticeMakesPerfect (
  practiceMakesPerfect,
  PracticeMakesPerfect (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Practiced))

newtype PracticeMakesPerfect = PracticeMakesPerfect EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

practiceMakesPerfect :: EventCard PracticeMakesPerfect
practiceMakesPerfect = event PracticeMakesPerfect Cards.practiceMakesPerfect

instance RunMessage PracticeMakesPerfect where
  runMessage msg e@(PracticeMakesPerfect attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push
        $ search
          iid
          (toSource attrs)
          iid
          [fromTopOfDeck 9]
          (#skill <> CardWithTrait Practiced)
          (DeferSearchedToTarget $ toTarget attrs)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      committable <- filterM (getIsCommittable iid) cards
      player <- getPlayer iid
      let
        choices =
          [ targetLabel
            (toCardId card)
            [ skillTestModifiers
                attrs
                (toCardId card)
                [IfSuccessfulModifier ReturnToHandAfterTest, MustBeCommitted]
            , SkillTestCommitCard iid card
            ]
          | card <- committable
          ]
      push
        $ if null choices
          then chooseOne player [Label "No cards found" []]
          else chooseOne player choices
      pure e
    _ -> PracticeMakesPerfect <$> runMessage msg attrs
