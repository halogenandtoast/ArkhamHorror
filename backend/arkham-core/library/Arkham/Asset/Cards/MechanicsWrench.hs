module Arkham.Asset.Cards.MechanicsWrench (mechanicsWrench, MechanicsWrench (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Attack
import Arkham.Matcher
import Arkham.Modifier

newtype MechanicsWrench = MechanicsWrench AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mechanicsWrench :: AssetCard MechanicsWrench
mechanicsWrench = asset MechanicsWrench Cards.mechanicsWrench

instance HasAbilities MechanicsWrench where
  getAbilities (MechanicsWrench attrs) =
    [ controlledAbility attrs 1 (exists $ EnemyAt YourLocation <> EnemyCanAttack You)
        $ FastAbility (exhaust attrs)
    , controlledAbility
        attrs
        2
        (exists $ CanFightEnemy (attrs.ability 1) <> AttackedYouSinceTheEndOfYourLastTurn)
        fightAction_
    ]

instance RunMessage MechanicsWrench where
  runMessage msg a@(MechanicsWrench attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanAttack (InvestigatorWithId iid)
      chooseOne
        iid
        [targetLabel enemy [EnemyAttack $ enemyAttack enemy (attrs.ability 1) iid] | enemy <- enemies]

      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      skillTestModifiers (attrs.ability 2) iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemyMatch iid (attrs.ability 2) AttackedYouSinceTheEndOfYourLastTurn
      pure a
    _ -> MechanicsWrench <$> liftRunMessage msg attrs
