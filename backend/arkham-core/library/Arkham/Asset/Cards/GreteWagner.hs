module Arkham.Asset.Cards.GreteWagner (
  greteWagner,
  GreteWagner (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Discover
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype GreteWagner = GreteWagner AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greteWagner :: AssetCard GreteWagner
greteWagner = ally GreteWagner Cards.greteWagner (3, 2)

instance HasModifiersFor GreteWagner where
  getModifiersFor (InvestigatorTarget iid) (GreteWagner a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GreteWagner where
  getAbilities (GreteWagner a) =
    [ controlledAbility
        a
        1
        (ClueOnLocation <> exists (You <> InvestigatorCanDiscoverCluesAt YourLocation))
        $ ReactionAbility
          (EnemyDefeated #after You ByAny AnyEnemy)
          (exhaust a <> DamageCost (toSource a) (toTarget a) 1)
    ]

instance RunMessage GreteWagner where
  runMessage msg a@(GreteWagner attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toAbilitySource attrs 1) 1
      pure a
    _ -> GreteWagner <$> runMessage msg attrs
