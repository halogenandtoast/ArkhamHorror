module Arkham.Asset.Cards.BeatCop2 (
  BeatCop2 (..),
  beatCop2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype BeatCop2 = BeatCop2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop2 :: AssetCard BeatCop2
beatCop2 = ally BeatCop2 Cards.beatCop2 (3, 2)

instance HasModifiersFor BeatCop2 where
  getModifiersFor (InvestigatorTarget iid) (BeatCop2 a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities BeatCop2 where
  getAbilities (BeatCop2 x) =
    [ restrictedAbility x 1 (ControlsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ FastAbility
        $ Costs [exhaust x, DamageCost (toSource x) (toTarget x) 1]
    ]

instance RunMessage BeatCop2 where
  runMessage msg a@(BeatCop2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemies <- selectList $ EnemyAt $ locationWithInvestigator iid
      push
        $ chooseOrRunOne iid
        $ [ targetLabel eid [EnemyDamage eid $ nonAttack (toAbilitySource attrs 1) 1]
          | eid <- enemies
          ]
      pure a
    _ -> BeatCop2 <$> runMessage msg attrs
