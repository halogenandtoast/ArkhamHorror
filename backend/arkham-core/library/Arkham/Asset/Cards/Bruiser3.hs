module Arkham.Asset.Cards.Bruiser3 (bruiser3, Bruiser3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Armor, Firearm, Melee))

newtype Bruiser3 = Bruiser3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bruiser3 :: AssetCard Bruiser3
bruiser3 = asset Bruiser3 Cards.bruiser3

instance HasModifiersFor Bruiser3 where
  getModifiersFor (InvestigatorTarget iid) (Bruiser3 attrs) =
    pure
      $ toModifiers
        attrs
        [ CanSpendUsesAsResourceOnCardFromInvestigator
          (toId attrs)
          Resource
          (InvestigatorWithId iid)
          (oneOf [CardWithTrait t | t <- [Armor, Firearm, Melee]])
        | attrs `controlledBy` iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities Bruiser3 where
  getAbilities (Bruiser3 a) =
    [ controlledAbility
        a
        1
        (DuringSkillTest $ oneOf [SkillTestOnCardWithTrait t | t <- [Armor, Firearm, Melee]])
        $ FastAbility (assetUseCost a Resource 1)
    ]

instance RunMessage Bruiser3 where
  runMessage msg a@(Bruiser3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Bruiser3 $ attrs & usesL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Bruiser3 <$> liftRunMessage msg attrs
