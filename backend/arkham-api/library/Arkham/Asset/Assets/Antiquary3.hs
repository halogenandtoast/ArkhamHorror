module Arkham.Asset.Assets.Antiquary3 (antiquary3, Antiquary3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Trait (Trait (Favor, Relic, Ritual))

newtype Antiquary3 = Antiquary3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

antiquary3 :: AssetCard Antiquary3
antiquary3 = asset Antiquary3 Cards.antiquary3

instance HasModifiersFor Antiquary3 where
  getModifiersFor (Antiquary3 a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      controllerGets
        a
        [ CanSpendUsesAsResourceOnCardFromInvestigator
            a.id
            #resource
            (InvestigatorWithId iid)
            (oneOf [CardWithTrait t | t <- [Favor, Relic, Ritual]])
        ]

instance HasAbilities Antiquary3 where
  getAbilities (Antiquary3 a) =
    [ wantsSkillTest (YourSkillTest #any)
        $ controlledAbility a 1 (DuringSkillTest $ mapOneOf SkillTestOnCardWithTrait [Favor, Relic, Ritual])
        $ FastAbility (assetUseCost a #resource 1)
    ]

instance RunMessage Antiquary3 where
  runMessage msg a@(Antiquary3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Antiquary3 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Antiquary3 <$> liftRunMessage msg attrs
