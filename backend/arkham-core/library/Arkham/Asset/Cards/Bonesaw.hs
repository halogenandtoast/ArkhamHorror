module Arkham.Asset.Cards.Bonesaw (bonesaw, Bonesaw (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Matcher
import Arkham.Modifier

newtype Bonesaw = Bonesaw AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bonesaw :: AssetCard Bonesaw
bonesaw = asset Bonesaw Cards.bonesaw

instance HasAbilities Bonesaw where
  getAbilities (Bonesaw a) =
    [ restrictedAbility a 1 ControlsThis fightAction_
    , playerLimit PerGame
        $ controlledAbility
          a
          2
          (exists $ HealableInvestigator (a.ability 2) #damage $ InvestigatorAt YourLocation)
          actionAbility
    ]

instance RunMessage Bonesaw where
  runMessage msg a@(Bonesaw attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (SkillModifier #combat 2)
      chooseFightEnemy iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseOne
        iid
        [ Label
            "Take 1 damage to do +1 damage"
            [ Msg.assignDamage iid (attrs.ability 1) 1
            , Msg.skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
            ]
        , Label "Do not take damage" []
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 2) #damage $ colocatedWith iid
      when (notNull investigators) do
        chooseOne
          iid
          [ targetLabel
            iid'
            [ HealDamage (toTarget iid') (attrs.ability 2) 5
            , Msg.beginSkillTest iid (attrs.ability 2) (toTarget iid') #intellect (Fixed 4)
            ]
          | iid' <- investigators
          ]
      pure a
    FailedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      getSkillTestTarget >>= \case
        Just (InvestigatorTarget iid') -> push $ SufferTrauma iid' 1 0
        _ -> pure ()
      pure a
    _ -> Bonesaw <$> liftRunMessage msg attrs
