module Arkham.Asset.Assets.SwordCane (swordCane) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Actions (orActions)
import Arkham.Evade.Types
import Arkham.Fight.Types
import Arkham.Helpers.CombatTarget
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SwordCane = SwordCane AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swordCane :: AssetCard SwordCane
swordCane = asset SwordCane Cards.swordCane

instance HasAbilities SwordCane where
  getAbilities (SwordCane x) =
    [ controlled
        x
        1
        ( oneOf
            [ any_ [CanEvadeEnemy (x.ability 2), CanFightEnemy (x.ability 2)]
            , exists $ YourLocation <> LocationWithConcealedCard
            ]
        )
        $ freeReaction
        $ AssetEntersPlay #after (be x)
    , restricted x 2 ControlsThis $ ActionAbility (orActions [#fight, #evade]) Nothing (exhaust x <> ActionCost 1)
    ]

instance RunMessage SwordCane where
  runMessage msg a@(SwordCane attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payments -> do
      liftRunMessage (UseCardAbility iid (toSource attrs) 2 windows' payments) a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      canFight <- hasFightTargets source iid
      canEvade <- hasEvadeTargets source iid
      sid <- getRandom
      chooseOrRunOneM iid do
        when canEvade $ labeled "Evade" do
          chooseOneM iid do
            for_ [#willpower, #agility] \sk -> do
              skillLabeled sk $ chooseEvadeEnemyEdit sid iid source (\ce -> ce {chooseEvadeSkillType = sk, chooseEvadeIsAction = True, chooseEvadePayCost = False})
        when canFight $ labeled "Fight" do
          chooseOneM iid do
            for_ [#willpower, #combat] \sk -> do
              skillLabeled sk $ chooseFightEnemyEdit sid iid source (\cf -> cf {chooseFightSkillType = sk, chooseFightIsAction = True, chooseFightPayCost = False})
      pure a
    _ -> SwordCane <$> liftRunMessage msg attrs
