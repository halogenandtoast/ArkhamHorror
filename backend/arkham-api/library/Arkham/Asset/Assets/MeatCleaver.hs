module Arkham.Asset.Assets.MeatCleaver (meatCleaver, meatCleaverEffect, MeatCleaver (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype MeatCleaver = MeatCleaver AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaver :: AssetCard MeatCleaver
meatCleaver = asset MeatCleaver Cards.meatCleaver

instance HasAbilities MeatCleaver where
  getAbilities (MeatCleaver attrs) =
    [ withAdditionalCost (UpTo (Fixed 1) $ HorrorCost (toSource attrs) YouTarget 1)
        $ restrictedAbility attrs 1 ControlsThis fightAction_
    ]

paidHorror :: Payment -> Bool
paidHorror (HorrorPayment _) = True
paidHorror (Payments ps) = any paidHorror ps
paidHorror _ = False

instance RunMessage MeatCleaver where
  runMessage msg a@(MeatCleaver attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let source = attrs.ability 1
      sid <- getRandom
      remainingSanity <- field InvestigatorRemainingSanity iid
      let n = if remainingSanity <= 3 then 2 else 1
      skillTestModifiers sid attrs iid $ SkillModifier #combat n : [DamageDealt 1 | paidHorror payments]
      createCardEffect Cards.meatCleaver (effectMetaTarget sid) source iid
      chooseFightEnemy sid iid source
      pure a
    _ -> MeatCleaver <$> liftRunMessage msg attrs

newtype MeatCleaverEffect = MeatCleaverEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaverEffect :: EffectArgs -> MeatCleaverEffect
meatCleaverEffect = cardEffect MeatCleaverEffect Cards.meatCleaver

instance RunMessage MeatCleaverEffect where
  runMessage msg e@(MeatCleaverEffect attrs) = runQueueT $ case msg of
    EnemyDefeated _ _ source _ | attrs.source == source -> do
      case attrs.target of
        InvestigatorTarget iid -> do
          canBeHealed <- canHaveHorrorHealed attrs.source iid
          when canBeHealed do
            chooseOneM iid do
              labeled "Do not heal" nothing
              horrorLabeled iid $ push $ HealHorror (toTarget iid) attrs.source 1
          disable attrs
        _ -> error "Invalid target"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> MeatCleaverEffect <$> liftRunMessage msg attrs
