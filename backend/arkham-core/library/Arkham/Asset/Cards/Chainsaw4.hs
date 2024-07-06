module Arkham.Asset.Cards.Chainsaw4 (chainsaw4, Chainsaw4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Fight
import Arkham.Prelude

newtype Chainsaw4 = Chainsaw4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chainsaw4 :: AssetCard Chainsaw4
chainsaw4 = asset Chainsaw4 Cards.chainsaw4

instance HasAbilities Chainsaw4 where
  getAbilities (Chainsaw4 a) = [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Supply 1]

instance RunMessage Chainsaw4 where
  runMessage msg a@(Chainsaw4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseFight <- toMessage <$> mkChooseFight iid (attrs.ability 1)
      pushAll
        [ skillTestModifiers attrs iid [SkillModifier #combat 2, DamageDealt 2]
        , chooseFight
        ]
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label "Place 1 supply on Chainsaw" [AddUses (attrs.ability 1) (toId attrs) Supply 1]
              , Label "Deal 1 damage to the attacked enemy" [EnemyDamage eid $ nonAttack (attrs.ability 1) 1]
              ]
        _ -> error "invalid call"
      pure a
    _ -> Chainsaw4 <$> runMessage msg attrs
