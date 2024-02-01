module Arkham.Asset.Cards.DecoratedSkull3 (
  decoratedSkull3,
  DecoratedSkull3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (
  AssetDefeated,
  EnemyDefeated,
  InvestigatorDefeated,
 )
import Arkham.Matcher

newtype DecoratedSkull3 = DecoratedSkull3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

decoratedSkull3 :: AssetCard DecoratedSkull3
decoratedSkull3 = asset DecoratedSkull3 Cards.decoratedSkull3

instance HasAbilities DecoratedSkull3 where
  getAbilities (DecoratedSkull3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( OrWindowMatcher
              [ EnemyDefeated #after Anyone ByAny AnyEnemy
              , InvestigatorDefeated #after ByAny Anyone
              , AssetDefeated #after ByAny AllyAsset
              ]
          )
          Free
    , restrictedAbility a 2 ControlsThis (actionAbilityWithCost $ UpTo 3 $ assetUseCost a Charge 1)
    ]

getUsesPaid :: Payment -> Int
getUsesPaid (UsesPayment n) = n
getUsesPaid (Payments ps) = sum $ map getUsesPaid ps
getUsesPaid _ = 0

instance RunMessage DecoratedSkull3 where
  runMessage msg a@(DecoratedSkull3 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AddUses (toId attrs) Charge 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (getUsesPaid -> n) -> do
      drawing <- drawCards iid (toAbilitySource attrs 2) n
      pushAll [drawing, TakeResources iid n (toAbilitySource attrs 2) False]
      pure a
    _ -> DecoratedSkull3 <$> runMessage msg attrs
