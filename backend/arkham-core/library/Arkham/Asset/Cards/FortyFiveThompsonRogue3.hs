module Arkham.Asset.Cards.FortyFiveThompsonRogue3
  ( fortyFiveThompsonRogue3
  , FortyFiveThompsonRogue3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( EnemyFight )
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype FortyFiveThompsonRogue3 = FortyFiveThompsonRogue3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompsonRogue3 :: AssetCard FortyFiveThompsonRogue3
fortyFiveThompsonRogue3 =
  asset FortyFiveThompsonRogue3 Cards.fortyFiveThompsonRogue3

instance HasAbilities FortyFiveThompsonRogue3 where
  getAbilities (FortyFiveThompsonRogue3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage FortyFiveThompsonRogue3 where
  runMessage msg a@(FortyFiveThompsonRogue3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 1, SkillModifier SkillCombat 2]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ n
      | useCount (assetUses attrs) > 0 -> do
        mSkillTestTarget <- getSkillTestTarget
        case mSkillTestTarget of
          Just (EnemyTarget eid) -> do
            fightValue <- field EnemyFight eid
            when (n >= fightValue) $ do
              enemies <-
                selectList $ EnemyAt (locationWithInvestigator iid) <> NotEnemy
                  (EnemyWithId eid)
              push
                $ chooseOrRunOne iid
                $ Label "Do not damage any enemies" []
                : [ targetLabel
                      eid'
                      [ SpendUses (toTarget attrs) Ammo 1
                      , InvestigatorDamageEnemy iid eid' (toSource attrs)
                      ]
                  | eid' <- enemies
                  ]
          _ -> pure ()
        pure a
    _ -> FortyFiveThompsonRogue3 <$> runMessage msg attrs
