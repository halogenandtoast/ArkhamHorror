module Arkham.Asset.Assets.SwordCaneDesignedByTheCouncilOfPolls2 (swordCaneDesignedByTheCouncilOfPolls2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Evade
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier (ModifierType(..))
import Arkham.Message.Lifted.Choose

newtype SwordCaneDesignedByTheCouncilOfPolls2 = SwordCaneDesignedByTheCouncilOfPolls2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swordCaneDesignedByTheCouncilOfPolls2 :: AssetCard SwordCaneDesignedByTheCouncilOfPolls2
swordCaneDesignedByTheCouncilOfPolls2 = asset SwordCaneDesignedByTheCouncilOfPolls2 Cards.swordCaneDesignedByTheCouncilOfPolls2

instance HasAbilities SwordCaneDesignedByTheCouncilOfPolls2 where
  getAbilities (SwordCaneDesignedByTheCouncilOfPolls2 x) =
    [ controlled x 1 (any_ [CanEvadeEnemy (x.ability 2), CanFightEnemy (x.ability 2), EnemyIsEngagedWith You <> EnemyCanBeDamagedBySource (x.ability 2)])
        $ freeReaction
        $ AssetEntersPlay #after (be x)
    , displayAsAction $ restricted x 2 ControlsThis $ fightAction $ exhaust x
    , displayAsAction $ restricted x 2 ControlsThis $ evadeAction $ exhaust x
    ]

instance RunMessage SwordCaneDesignedByTheCouncilOfPolls2 where
  runMessage msg a@(SwordCaneDesignedByTheCouncilOfPolls2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payments -> do
      enemies <- select $ enemyEngagedWith iid <> EnemyCanBeDamagedBySource (attrs.ability 2)

      chooseOneM iid do
        labeled "Do not deal damage" nothing
        targets enemies (nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1)

      push $ UseCardAbility iid (toSource attrs) 2 windows' payments
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      fightableEnemies <- select $ CanFightEnemy source
      evadeableEnemies <- select $ CanEvadeEnemy source

      sid <- getRandom

      skillTestModifier sid source iid (AnySkillValue 1)

      chooseOrRunOneM iid do
        unless (null evadeableEnemies) $ labeled "Evade" do
          chooseOneM iid do
            for_ [#willpower, #agility] \sk -> do
              skillLabeled sk $ chooseEvadeEnemyEdit sid iid source (Arkham.Evade.withSkillType sk)
        unless (null fightableEnemies) $ labeled "Fight" do
          chooseOneM iid do
            for_ [#willpower, #combat] \sk -> do
              skillLabeled sk $ chooseFightEnemyEdit sid iid source (Arkham.Fight.withSkillType sk)
      pure a
    _ -> SwordCaneDesignedByTheCouncilOfPolls2 <$> liftRunMessage msg attrs
