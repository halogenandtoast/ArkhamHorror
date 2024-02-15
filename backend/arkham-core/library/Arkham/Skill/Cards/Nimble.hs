module Arkham.Skill.Cards.Nimble (
  nimble,
  Nimble (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Movement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Metadata = Metadata {moveCount :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Nimble = Nimble (SkillAttrs `With` Metadata)
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nimble :: SkillCard Nimble
nimble = skill (Nimble . (`with` Metadata 0)) Cards.nimble

instance RunMessage Nimble where
  runMessage msg s@(Nimble (attrs `With` meta)) = case msg of
    After (PassedSkillTest _ _ _ SkillTestInitiatorTarget {} _ (min 3 -> n)) | n > 0 -> do
      let iid = skillOwner attrs
      connectingLocation <- notNull <$> getAccessibleLocations iid attrs
      if connectingLocation
        then do
          push $ ResolveSkill (toId attrs)
          pure $ Nimble $ attrs `with` Metadata n
        else pure s
    ResolveSkill sId | sId == toId attrs && moveCount meta > 0 -> do
      let iid = skillOwner attrs
      connectingLocations <- getAccessibleLocations iid attrs
      player <- getPlayer iid
      unless (null connectingLocations) $ do
        push
          $ chooseOne player
          $ Label "Do not move" []
          : [ targetLabel
              location
              [MoveTo $ move (toSource attrs) iid location, ResolveSkill (toId attrs)]
            | location <- connectingLocations
            ]
      pure $ Nimble $ attrs `with` Metadata (moveCount meta - 1)
    _ -> Nimble . (`with` meta) <$> runMessage msg attrs
