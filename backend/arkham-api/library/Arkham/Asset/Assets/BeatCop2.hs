module Arkham.Asset.Assets.BeatCop2 (BeatCop2 (..), beatCop2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude

newtype BeatCop2 = BeatCop2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop2 :: AssetCard BeatCop2
beatCop2 = ally BeatCop2 Cards.beatCop2 (3, 2)

instance HasModifiersFor BeatCop2 where
  getModifiersFor (BeatCop2 a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities BeatCop2 where
  getAbilities (BeatCop2 x) =
    [ controlledAbility x 1 (exists (EnemyAt YourLocation) <> CanDealDamage)
        $ FastAbility
        $ Costs [exhaust x, DamageCost (toSource x) (toTarget x) 1]
    ]

instance RunMessage BeatCop2 where
  runMessage msg a@(BeatCop2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAtLocationWith iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ targetLabels enemies (only . nonAttackEnemyDamage (toAbilitySource attrs 1) 1)
      pure a
    _ -> BeatCop2 <$> runMessage msg attrs
