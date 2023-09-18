module Arkham.Asset.Cards.Stealth3 (
  stealth3,
  Stealth3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (DuringTurn)
import Arkham.SkillTestResult

newtype Stealth3 = Stealth3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stealth3 :: AssetCard Stealth3
stealth3 = asset Stealth3 Cards.stealth3

instance HasAbilities Stealth3 where
  getAbilities (Stealth3 attrs) =
    [ restrictedAbility attrs 1 (DuringTurn You <> ControlsThis)
        $ FastAbility' (exhaust attrs) (Just #evade)
    ]

instance RunMessage Stealth3 where
  runMessage msg a@(Stealth3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ chooseEvadeEnemyWithTarget iid (toAbilitySource attrs 1) attrs #agility
      pure a
    ChosenEvadeEnemy source@(isSource attrs -> True) eid -> do
      push $ skillTestModifier source eid (EnemyEvade (-2))
      pure a
    AfterSkillTestEnds (isSource attrs -> True) target@(EnemyTarget eid) (SucceededBy _ _) -> do
      let iid = getController attrs
      canDisengage <- iid <=~> InvestigatorCanDisengage
      pushAll
        $ [turnModifier attrs target (EnemyCannotEngage iid)]
          <> [DisengageEnemy iid eid | canDisengage]
      pure a
    _ -> Stealth3 <$> runMessage msg attrs
