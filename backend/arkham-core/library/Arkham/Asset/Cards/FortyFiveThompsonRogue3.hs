module Arkham.Asset.Cards.FortyFiveThompsonRogue3 (
  fortyFiveThompsonRogue3,
  FortyFiveThompsonRogue3 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyFight)
import Arkham.Enemy.Types (Field (..))
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype FortyFiveThompsonRogue3 = FortyFiveThompsonRogue3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompsonRogue3 :: AssetCard FortyFiveThompsonRogue3
fortyFiveThompsonRogue3 = asset FortyFiveThompsonRogue3 Cards.fortyFiveThompsonRogue3

instance HasAbilities FortyFiveThompsonRogue3 where
  getAbilities (FortyFiveThompsonRogue3 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage FortyFiveThompsonRogue3 where
  runMessage msg a@(FortyFiveThompsonRogue3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 2], chooseFight]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | attrs.use Ammo > 0 -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          fightValue <- fieldJust EnemyFight eid
          when (n >= fightValue) $ do
            enemies <- select $ enemyAtLocationWith iid <> NotEnemy (EnemyWithId eid)
            canDealDamage <- withoutModifier iid CannotDealDamage
            player <- getPlayer iid
            push
              $ chooseOrRunOne player
              $ Label "Do not damage any enemies" []
              : [ targetLabel
                  eid'
                  [ SpendUses (attrs.ability 1) (toTarget attrs) Ammo 1
                  , InvestigatorDamageEnemy iid eid' (toSource attrs)
                  ]
                | canDealDamage
                , eid' <- enemies
                ]
        _ -> pure ()
      pure a
    _ -> FortyFiveThompsonRogue3 <$> runMessage msg attrs
