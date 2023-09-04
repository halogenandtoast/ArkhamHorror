module Arkham.Location.Cards.UnvisitedIsleHauntedSpring (
  unvisitedIsleHauntedSpring,
  UnvisitedIsleHauntedSpring (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.CampaignLogKey
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleHauntedSpring = UnvisitedIsleHauntedSpring LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleHauntedSpring :: LocationCard UnvisitedIsleHauntedSpring
unvisitedIsleHauntedSpring = location UnvisitedIsleHauntedSpring Cards.unvisitedIsleHauntedSpring 2 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleHauntedSpring where
  getModifiersFor target (UnvisitedIsleHauntedSpring attrs)
    | attrs `isTarget` target
    , not (locationRevealed attrs) = do
        sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
        isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
        if sidedWithLodge
          then pure [toModifier attrs Blocked | isLit]
          else pure [toModifier attrs Blocked | not isLit]
  getModifiersFor _ _ = pure []

instance HasAbilities UnvisitedIsleHauntedSpring where
  getAbilities (UnvisitedIsleHauntedSpring attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility (Just Action.Circle) $ ActionCost 1
      , haunted "You must either discard an asset you control, or take 1 damage" attrs 2
      ]

instance RunMessage UnvisitedIsleHauntedSpring where
  runMessage msg l@(UnvisitedIsleHauntedSpring attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      circleTest iid attrs attrs #intellect #agility 9
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      push $
        chooseOrRunOne iid $
          [ Label "Discard an asset you control" [ChooseAndDiscardAsset iid (toSource attrs) AnyAsset]
          | hasAssets
          ]
            <> [Label "Take 1 damage" [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]]
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestTarget {} _ _ -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleHauntedSpring <$> runMessage msg attrs
