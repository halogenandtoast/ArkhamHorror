module Arkham.Asset.Cards.Chainsaw4 (
  chainsaw4,
  Chainsaw4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.SkillType

newtype Chainsaw4 = Chainsaw4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

chainsaw4 :: AssetCard Chainsaw4
chainsaw4 = asset Chainsaw4 Cards.chainsaw4

instance HasAbilities Chainsaw4 where
  getAbilities (Chainsaw4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility ([Action.Fight])
        $ UseCost (AssetWithId $ toId a) Supply 1
    ]

instance RunMessage Chainsaw4 where
  runMessage msg a@(Chainsaw4 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            [SkillModifier SkillCombat 2, DamageDealt 2]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (EnemyTarget eid) -> do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label
                  "Place 1 supply on Chainsaw"
                  [AddUses (toId attrs) Supply 1]
              , Label
                  "Deal 1 damage to the attacked enemy"
                  [EnemyDamage eid $ nonAttack source 1]
              ]
        _ -> error "invalid call"
      pure a
    _ -> Chainsaw4 <$> runMessage msg attrs
