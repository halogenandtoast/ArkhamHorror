module Arkham.Asset.Cards.FeedTheMind3
  ( feedTheMind3
  , FeedTheMind3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype FeedTheMind3 = FeedTheMind3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheMind3 :: AssetCard FeedTheMind3
feedTheMind3 = asset FeedTheMind3 Cards.feedTheMind3

instance HasAbilities FeedTheMind3 where
  getAbilities (FeedTheMind3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility Nothing
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage FeedTheMind3 where
  runMessage msg a@(FeedTheMind3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest
        iid
        (toAbilitySource attrs 1)
        (InvestigatorTarget iid)
        SkillIntellect
        0
      pure a
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget{} _ n
      -> do
        drawing <- drawCards iid attrs n
        pushAll
          [ drawing
          , HandleTargetChoice iid (toSource attrs) (toTarget attrs)
          ]
        pure a
    HandleTargetChoice iid (isSource attrs -> True) _ -> do
      handSize <- field InvestigatorHandSize iid
      handCount <- fieldMap InvestigatorHand length iid
      let n = handCount - handSize
      when (n > 0) $ push $ InvestigatorAssignDamage
        iid
        (toSource attrs)
        DamageAny
        0
        n
      pure a
    _ -> FeedTheMind3 <$> runMessage msg attrs
