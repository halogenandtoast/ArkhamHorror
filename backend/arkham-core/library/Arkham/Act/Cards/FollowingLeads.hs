module Arkham.Act.Cards.FollowingLeads (
  FollowingLeads (..),
  followingLeads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Trait (Trait (Lead))

newtype FollowingLeads = FollowingLeads ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followingLeads :: ActCard FollowingLeads
followingLeads = act (2, A) FollowingLeads Cards.followingLeads Nothing

instance HasAbilities FollowingLeads where
  getAbilities (FollowingLeads x) =
    [ restrictedAbility x 1 (AssetCount 2 $ AssetWithTrait Lead <> AssetWithClues (atLeast 1))
        $ Objective
        $ ReactionAbility (RoundEnds #when) Free
    ]

instance RunMessage FollowingLeads where
  runMessage msg a@(FollowingLeads attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pure a
    _ -> FollowingLeads <$> runMessage msg attrs
