module Arkham.Skill.Cards.Intrepid
  ( intrepid
  , Intrepid(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Message
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Intrepid = Intrepid SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intrepid :: SkillCard Intrepid
intrepid = skill Intrepid Cards.intrepid

instance RunMessage Intrepid where
  runMessage msg s@(Intrepid attrs) = case msg of
    PassedSkillTest iid (Just Action.Investigate) _ (isTarget attrs -> True) _ _
      -> do
        intrepidAsset <- genCard Assets.intrepid
        pushAll
          [ RemoveSkill $ toId attrs
          , CreateAssetAt intrepidAsset (InPlayArea iid)
          ]
        pure s
    _ -> Intrepid <$> runMessage msg attrs
