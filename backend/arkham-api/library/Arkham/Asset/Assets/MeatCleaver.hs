module Arkham.Asset.Assets.MeatCleaver (meatCleaver, meatCleaverEffect) where

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
        $ restricted attrs 1 ControlsThis fightAction_
    ]

instance RunMessage MeatCleaver where
  runMessage msg a@(MeatCleaver attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let source = attrs.ability 1
      sid <- getRandom
      remainingSanity <- field InvestigatorRemainingSanity iid
      let n = if remainingSanity <= 3 then 2 else 1
      skillTestModifiers sid source iid $ SkillModifier #combat n : [DamageDealt 1 | payments.horror > 0]
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
      for_ attrs.target.investigator \iid -> do
        whenM (canHaveHorrorHealed attrs.source iid) do
          chooseOneM iid do
            labeled "Do not heal" nothing
            horrorLabeled iid $ healHorror iid attrs.source 1
      disableReturn e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> MeatCleaverEffect <$> liftRunMessage msg attrs
