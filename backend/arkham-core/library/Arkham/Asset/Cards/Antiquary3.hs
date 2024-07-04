module Arkham.Asset.Cards.Antiquary3 (antiquary3, Antiquary3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Favor, Relic, Ritual))

newtype Antiquary3 = Antiquary3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

antiquary3 :: AssetCard Antiquary3
antiquary3 = asset Antiquary3 Cards.antiquary3

instance HasModifiersFor Antiquary3 where
  getModifiersFor (InvestigatorTarget iid) (Antiquary3 attrs) =
    pure
      $ toModifiers
        attrs
        [ CanSpendUsesAsResourceOnCardFromInvestigator
          (toId attrs)
          Resource
          (InvestigatorWithId iid)
          (oneOf [CardWithTrait t | t <- [Favor, Relic, Ritual]])
        | attrs `controlledBy` iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities Antiquary3 where
  getAbilities (Antiquary3 a) =
    [ controlledAbility
        a
        1
        (DuringSkillTest $ oneOf [SkillTestOnCardWithTrait t | t <- [Favor, Relic, Ritual]])
        $ FastAbility (assetUseCost a Resource 1)
    ]

instance RunMessage Antiquary3 where
  runMessage msg a@(Antiquary3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Antiquary3 $ attrs & usesL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Antiquary3 <$> liftRunMessage msg attrs
