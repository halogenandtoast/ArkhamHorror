module Arkham.Asset.Cards.Sleuth3 (sleuth3, Sleuth3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Charm, Tactic, Tome))

newtype Sleuth3 = Sleuth3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleuth3 :: AssetCard Sleuth3
sleuth3 = asset Sleuth3 Cards.sleuth3

instance HasModifiersFor Sleuth3 where
  getModifiersFor (InvestigatorTarget iid) (Sleuth3 attrs) =
    pure
      $ toModifiers
        attrs
        [ CanSpendUsesAsResourceOnCardFromInvestigator
          (toId attrs)
          Resource
          (InvestigatorWithId iid)
          (oneOf [CardWithTrait t | t <- [Charm, Tactic, Tome]])
        | attrs `controlledBy` iid
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities Sleuth3 where
  getAbilities (Sleuth3 a) =
    [ controlledAbility
        a
        1
        (DuringSkillTest $ oneOf [SkillTestOnCardWithTrait t | t <- [Charm, Tactic, Tome]])
        $ FastAbility (assetUseCost a Resource 1)
    ]

instance RunMessage Sleuth3 where
  runMessage msg a@(Sleuth3 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Sleuth3 $ attrs & tokensL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Sleuth3 <$> liftRunMessage msg attrs
