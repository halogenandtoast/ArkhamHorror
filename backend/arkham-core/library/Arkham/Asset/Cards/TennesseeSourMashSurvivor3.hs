module Arkham.Asset.Cards.TennesseeSourMashSurvivor3 (
  tennesseeSourMashSurvivor3,
  TennesseeSourMashSurvivor3 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Prelude

newtype TennesseeSourMashSurvivor3 = TennesseeSourMashSurvivor3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tennesseeSourMashSurvivor3 :: AssetCard TennesseeSourMashSurvivor3
tennesseeSourMashSurvivor3 = asset TennesseeSourMashSurvivor3 Cards.tennesseeSourMashSurvivor3

instance HasAbilities TennesseeSourMashSurvivor3 where
  getAbilities (TennesseeSourMashSurvivor3 a) =
    [ controlledAbility a 1 (DuringSkillTest (SkillTestOnTreachery AnyTreachery))
        $ FastAbility (exhaust a <> assetUseCost a Supply 1)
    , restrictedAbility a 2 ControlsThis $ fightAction (discardCost a)
    ]

instance RunMessage TennesseeSourMashSurvivor3 where
  runMessage msg a@(TennesseeSourMashSurvivor3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier attrs iid (SkillModifier #willpower 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifier source iid (SkillModifier #combat 3)
        , chooseFight
        ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (EnemyTarget eid) -> do
          nonElite <- eid <=~> NonEliteEnemy
          when nonElite $ push $ EnemyEvaded iid eid
        _ -> error "impossible"
      pure a
    _ -> TennesseeSourMashSurvivor3 <$> runMessage msg attrs
