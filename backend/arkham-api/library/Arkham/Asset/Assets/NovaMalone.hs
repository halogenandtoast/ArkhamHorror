module Arkham.Asset.Assets.NovaMalone (novaMalone) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.TheMidwinterGala.Helpers

newtype NovaMalone = NovaMalone AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

novaMalone :: AssetCard NovaMalone
novaMalone = allyWith NovaMalone Cards.novaMalone (3, 1) noSlots

instance HasModifiersFor NovaMalone where
  getModifiersFor (NovaMalone a) = handleSpellbound a

instance HasAbilities NovaMalone where
  getAbilities (NovaMalone a) =
    [ fightAbility a 1 (exhaust a) ControlsThis
    , playerLimit PerRound
        $ reaction a 2 ControlsThis Free (EnemyDefeated #after You ByAny AnyEnemy)
    ]

instance RunMessage NovaMalone where
  runMessage msg a@(NovaMalone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers
        sid
        (attrs.ability 1)
        iid
        [ BaseSkillOfCalculated
            #combat
            (MaxCalculation (Fixed 7) (InvestigatorFieldCalculation iid InvestigatorResources))
        , DamageDealt 1
        ]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResources iid (attrs.ability 2) 1
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ NovaMalone $ attrs & flippedL .~ True & visibleL .~ False & setMeta True
    Flip _ _ (isTarget attrs -> True) -> do
      let flipped = not $ view flippedL attrs
      pure $ NovaMalone $ attrs & flippedL .~ flipped & visibleL .~ True & setMeta False
    _ -> NovaMalone <$> liftRunMessage msg attrs
