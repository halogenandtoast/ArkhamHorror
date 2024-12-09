module Arkham.Asset.Assets.Bruiser3 (bruiser3, Bruiser3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Trait (Trait (Armor, Firearm, Melee))

newtype Bruiser3 = Bruiser3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bruiser3 :: AssetCard Bruiser3
bruiser3 = asset Bruiser3 Cards.bruiser3

instance HasModifiersFor Bruiser3 where
  getModifiersFor (Bruiser3 a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      controllerGets
        a
        [ CanSpendUsesAsResourceOnCardFromInvestigator
            a.id
            #resource
            (InvestigatorWithId iid)
            (oneOf [CardWithTrait t | t <- [Armor, Firearm, Melee]])
        ]

instance HasAbilities Bruiser3 where
  getAbilities (Bruiser3 a) =
    [ wantsSkillTest (YourSkillTest #any)
        $ controlledAbility a 1 (DuringSkillTest $ mapOneOf SkillTestOnCardWithTrait [Armor, Firearm, Melee])
        $ FastAbility (assetUseCost a #resource 1)
    ]

instance RunMessage Bruiser3 where
  runMessage msg a@(Bruiser3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Bruiser3 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Bruiser3 <$> liftRunMessage msg attrs
