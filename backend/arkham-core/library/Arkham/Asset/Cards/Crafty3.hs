module Arkham.Asset.Cards.Crafty3 (crafty3, Crafty3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Insight, Tool, Trick))

newtype Crafty3 = Crafty3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crafty3 :: AssetCard Crafty3
crafty3 = asset Crafty3 Cards.crafty3

instance HasModifiersFor Crafty3 where
  getModifiersFor (InvestigatorTarget iid) (Crafty3 attrs) =
    pure
      $ toModifiers
        attrs
        [ CanSpendUsesAsResourceOnCardFromInvestigator
          (toId attrs)
          Resource
          (InvestigatorWithId iid)
          (oneOf [CardWithTrait t | t <- [Insight, Tool, Trick]])
        | attrs `controlledBy` iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities Crafty3 where
  getAbilities (Crafty3 a) =
    [ controlledAbility
        a
        1
        (DuringSkillTest $ oneOf [SkillTestOnCardWithTrait t | t <- [Insight, Tool, Trick]])
        $ FastAbility (assetUseCost a Resource 1)
    ]

instance RunMessage Crafty3 where
  runMessage msg a@(Crafty3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Crafty3 $ attrs & tokensL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Crafty3 <$> liftRunMessage msg attrs
