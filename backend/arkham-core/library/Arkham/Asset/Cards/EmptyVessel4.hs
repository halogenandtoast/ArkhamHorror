module Arkham.Asset.Cards.EmptyVessel4 (
  emptyVessel4,
  EmptyVessel4 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype EmptyVessel4 = EmptyVessel4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emptyVessel4 :: AssetCard EmptyVessel4
emptyVessel4 = asset EmptyVessel4 Cards.emptyVessel4

instance HasAbilities EmptyVessel4 where
  getAbilities (EmptyVessel4 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ freeReaction (Matcher.EnemyDefeated #after You ByAny AnyEnemy)
    , restrictedAbility
        attrs
        2
        ( ControlsThis
            <> exists (AssetWithId (toId attrs) <> AssetWithUseCount Charge (atLeast 3))
            <> exists (You <> InvestigatorWithBondedCard (cardIs Cards.wishEater))
        )
        $ FastAbility Free
    ]

instance RunMessage EmptyVessel4 where
  runMessage msg a@(EmptyVessel4 attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ AddUses (toId a) Charge 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      wishEater <- fromJustNote "missing wish eater" . listToMaybe <$> searchBonded iid Cards.wishEater
      -- Rest is handled by Wish Eater
      push $ PutCardIntoPlay iid wishEater Nothing NoPayment []
      pure a
    _ -> EmptyVessel4 <$> runMessage msg attrs
