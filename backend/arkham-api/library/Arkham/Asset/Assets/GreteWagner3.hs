module Arkham.Asset.Assets.GreteWagner3 (greteWagner3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Discover
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype GreteWagner3 = GreteWagner3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greteWagner3 :: AssetCard GreteWagner3
greteWagner3 = ally GreteWagner3 Cards.greteWagner3 (4, 2)

instance HasModifiersFor GreteWagner3 where
  getModifiersFor (GreteWagner3 a) = controllerGets a [SkillModifier #combat 1, SkillModifier #intellect 1]

instance HasAbilities GreteWagner3 where
  getAbilities (GreteWagner3 a) =
    [ controlled
        a
        1
        (ClueOnLocation <> youExist (InvestigatorCanDiscoverCluesAt YourLocation))
        $ triggered
          (EnemyDefeated #after You ByAny AnyEnemy)
          (exhaust a <> DamageCost (toSource a) (toTarget a) 1)
    ]

instance RunMessage GreteWagner3 where
  runMessage msg a@(GreteWagner3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toAbilitySource attrs 1) 1
      pure a
    _ -> GreteWagner3 <$> runMessage msg attrs
