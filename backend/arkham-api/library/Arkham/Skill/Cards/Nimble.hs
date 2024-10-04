module Arkham.Skill.Cards.Nimble (nimble, Nimble (..)) where

import Arkham.Game.Helpers
import Arkham.Movement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Metadata = Metadata {moveCount :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Nimble = Nimble (SkillAttrs `With` Metadata)
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nimble :: SkillCard Nimble
nimble = skill (Nimble . (`with` Metadata 0)) Cards.nimble

instance RunMessage Nimble where
  runMessage msg s@(Nimble (attrs `With` meta)) = runQueueT $ case msg of
    PassedSkillTest _ _ _ SkillTestInitiatorTarget {} _ (min 3 -> n) | n > 0 -> do
      let iid = skillOwner attrs
      connectingLocation <- notNull <$> getAccessibleLocations iid attrs
      if connectingLocation
        then do
          afterSkillTest $ push $ ResolveSkill (toId attrs)
          pure $ Nimble $ attrs `with` Metadata n
        else pure s
    ResolveSkill sId | sId == toId attrs && moveCount meta > 0 -> do
      let iid = skillOwner attrs
      connectingLocations <- getAccessibleLocations iid attrs
      unless (null connectingLocations) $ do
        chooseOne iid
          $ Label "Do not move" []
          : [ targetLabel
              location
              [Move $ move (toSource attrs) iid location, ResolveSkill (toId attrs)]
            | location <- connectingLocations
            ]
      pure $ Nimble $ attrs `with` Metadata (moveCount meta - 1)
    _ -> Nimble . (`with` meta) <$> liftRunMessage msg attrs
