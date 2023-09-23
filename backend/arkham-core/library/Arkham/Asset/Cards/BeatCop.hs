module Arkham.Asset.Cards.BeatCop (
  BeatCop (..),
  beatCop,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype BeatCop = BeatCop AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop :: AssetCard BeatCop
beatCop = ally BeatCop Cards.beatCop (2, 2)

instance HasModifiersFor BeatCop where
  getModifiersFor (InvestigatorTarget iid) (BeatCop a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities BeatCop where
  getAbilities (BeatCop x) =
    [ restrictedAbility x 1 (ControlsThis <> enemyExists (EnemyAt YourLocation))
        $ FastAbility (DiscardCost FromPlay $ toTarget x)
    ]

instance RunMessage BeatCop where
  runMessage msg a@(BeatCop attrs) = case msg of
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let source = toAbilitySource attrs 1
      enemies <- selectList $ enemyAtLocationWith iid
      push $ chooseOrRunOne iid $ targetLabels enemies (only . assignEnemyDamage (nonAttack source 1))
      pure a
    _ -> BeatCop <$> runMessage msg attrs
