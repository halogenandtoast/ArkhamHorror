module Arkham.Asset.Assets.GreteWagner (greteWagner) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Discover
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype GreteWagner = GreteWagner AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greteWagner :: AssetCard GreteWagner
greteWagner = ally GreteWagner Cards.greteWagner (3, 2)

instance HasModifiersFor GreteWagner where
  getModifiersFor (GreteWagner a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities GreteWagner where
  getAbilities (GreteWagner a) =
    [ controlled
        a
        1
        (ClueOnLocation <> exists (You <> InvestigatorCanDiscoverCluesAt YourLocation))
        $ triggered
          (EnemyDefeated #after You ByAny AnyEnemy)
          (exhaust a <> DamageCost (toSource a) (toTarget a) 1)
    ]

instance RunMessage GreteWagner where
  runMessage msg a@(GreteWagner attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toAbilitySource attrs 1) 1
      pure a
    _ -> GreteWagner <$> runMessage msg attrs
