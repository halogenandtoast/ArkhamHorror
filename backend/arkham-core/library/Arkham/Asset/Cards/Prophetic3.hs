module Arkham.Asset.Cards.Prophetic3 (prophetic3, Prophetic3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Fortune, Spell, Spirit))

newtype Prophetic3 = Prophetic3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prophetic3 :: AssetCard Prophetic3
prophetic3 = asset Prophetic3 Cards.prophetic3

instance HasModifiersFor Prophetic3 where
  getModifiersFor (InvestigatorTarget iid) (Prophetic3 attrs) =
    pure
      $ toModifiers
        attrs
        [ CanSpendUsesAsResourceOnCardFromInvestigator
          (toId attrs)
          Resource
          (InvestigatorWithId iid)
          (oneOf [CardWithTrait t | t <- [Fortune, Spell, Spirit]])
        | attrs `controlledBy` iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities Prophetic3 where
  getAbilities (Prophetic3 a) =
    [ controlledAbility
        a
        1
        (DuringSkillTest $ oneOf [SkillTestOnCardWithTrait t | t <- [Fortune, Spell, Spirit]])
        $ FastAbility (assetUseCost a Resource 1)
    ]

instance RunMessage Prophetic3 where
  runMessage msg a@(Prophetic3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Prophetic3 $ attrs & tokensL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Prophetic3 <$> liftRunMessage msg attrs
