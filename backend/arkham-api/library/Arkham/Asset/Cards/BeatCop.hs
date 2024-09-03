module Arkham.Asset.Cards.BeatCop (BeatCop (..), beatCop) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude

newtype BeatCop = BeatCop AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop :: AssetCard BeatCop
beatCop = ally BeatCop Cards.beatCop (2, 2)

instance HasModifiersFor BeatCop where
  getModifiersFor (InvestigatorTarget iid) (BeatCop a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities BeatCop where
  getAbilities (BeatCop x) =
    [ controlledAbility x 1 (exists (EnemyAt YourLocation) <> CanDealDamage)
        $ FastAbility (DiscardCost FromPlay $ toTarget x)
    ]

instance RunMessage BeatCop where
  runMessage msg a@(BeatCop attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      enemies <- select $ enemyAtLocationWith iid
      player <- getPlayer iid
      push $ chooseOrRunOne player $ targetLabels enemies (only . assignEnemyDamage (nonAttack source 1))
      pure a
    _ -> BeatCop <$> runMessage msg attrs
