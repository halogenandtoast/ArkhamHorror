module Arkham.Asset.Assets.Crafty3 (crafty3, Crafty3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Trait (Trait (Insight, Tool, Trick))

newtype Crafty3 = Crafty3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crafty3 :: AssetCard Crafty3
crafty3 = asset Crafty3 Cards.crafty3

instance HasModifiersFor Crafty3 where
  getModifiersFor (Crafty3 a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      controllerGets
        a
        [ CanSpendUsesAsResourceOnCardFromInvestigator
            a.id
            #resource
            (InvestigatorWithId iid)
            (oneOf [CardWithTrait t | t <- [Insight, Tool, Trick]])
        ]

instance HasAbilities Crafty3 where
  getAbilities (Crafty3 a) =
    [ wantsSkillTest (YourSkillTest #any)
        $ controlledAbility a 1 (DuringSkillTest $ mapOneOf SkillTestOnCardWithTrait [Insight, Tool, Trick])
        $ FastAbility (assetUseCost a #resource 1)
    ]

instance RunMessage Crafty3 where
  runMessage msg a@(Crafty3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Crafty3 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Crafty3 <$> liftRunMessage msg attrs
