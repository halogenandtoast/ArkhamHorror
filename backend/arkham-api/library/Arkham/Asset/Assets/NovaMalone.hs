module Arkham.Asset.Assets.NovaMalone (novaMalone) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype NovaMalone = NovaMalone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

novaMalone :: AssetCard NovaMalone
novaMalone = allyWith NovaMalone Cards.novaMalone (3, 1) noSlots

instance HasAbilities NovaMalone where
  getAbilities (NovaMalone a) =
    [ fightAbility a 1 (exhaust a) ControlsThis
    , playerLimit PerRound
        $ reaction a 2 ControlsThis Free (EnemyDefeated #after You ByAny AnyEnemy)
    ]

instance RunMessage NovaMalone where
  runMessage msg a@(NovaMalone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- field InvestigatorResources iid
      let base = min 7 resources
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [BaseSkillOf #combat base, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResources iid (attrs.ability 2) 1
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ NovaMalone $ attrs & flippedL .~ True & visibleL .~ False
    Flip _ _ (isTarget attrs -> True) -> do
      let flipped = not $ view flippedL attrs
      pure $ NovaMalone $ attrs & flippedL .~ flipped & visibleL .~ True
    _ -> NovaMalone <$> liftRunMessage msg attrs
