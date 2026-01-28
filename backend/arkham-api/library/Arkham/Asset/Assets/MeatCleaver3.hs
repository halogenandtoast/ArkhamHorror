module Arkham.Asset.Assets.MeatCleaver3 (meatCleaver3, meatCleaver3Effect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype MeatCleaver3 = MeatCleaver3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaver3 :: AssetCard MeatCleaver3
meatCleaver3 = asset MeatCleaver3 Cards.meatCleaver3

instance HasAbilities MeatCleaver3 where
  getAbilities (MeatCleaver3 attrs) =
    [ withAdditionalCost (UpTo (Fixed 1) $ HorrorCost (toSource attrs) YouTarget 1)
        $ restricted attrs 1 ControlsThis fightAction_
    ]

instance RunMessage MeatCleaver3 where
  runMessage msg a@(MeatCleaver3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let source = attrs.ability 1
      sid <- getRandom
      remainingSanity <- field InvestigatorRemainingSanity iid
      let n = if remainingSanity <= 3 then 3 else 2
      skillTestModifiers sid source iid $ SkillModifier #combat n : [DamageDealt 1 | payments.horror > 0]
      createCardEffect Cards.meatCleaver3 (effectMetaTarget sid) source iid
      chooseFightEnemy sid iid source
      pure a
    _ -> MeatCleaver3 <$> liftRunMessage msg attrs

newtype MeatCleaver3Effect = MeatCleaver3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaver3Effect :: EffectArgs -> MeatCleaver3Effect
meatCleaver3Effect = cardEffect MeatCleaver3Effect Cards.meatCleaver3

instance RunMessage MeatCleaver3Effect where
  runMessage msg e@(MeatCleaver3Effect attrs) = runQueueT $ case msg of
    EnemyDefeated _ _ source _ | attrs.source == source -> do
      for_ attrs.target.investigator \iid -> do
        whenM (canHaveHorrorHealed attrs.source iid) do
          chooseOneM iid do
            labeled "Do not heal" nothing
            horrorLabeled iid $ healHorror iid attrs.source 1
      disableReturn e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> MeatCleaver3Effect <$> liftRunMessage msg attrs
