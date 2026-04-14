module Arkham.Asset.Assets.Becky2 (becky2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Matcher

newtype Becky2 = Becky2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

becky2 :: AssetCard Becky2
becky2 = asset Becky2 Cards.becky2

instance HasModifiersFor Becky2 where
  getModifiersFor (Becky2 a) = do
    for_ a.controller \iid -> do
      modifiedWhen_
        a
        a.ready
        (AbilityTarget iid $ AbilityRef (toSource a) 1)
        [CanModify $ EnemyFightActionCriteria $ CriteriaOverride canFightIgnoreAloof]

instance HasAbilities Becky2 where
  getAbilities (Becky2 a) =
    [ skillTestAbility $ controlled_ a 1 $ fightAction (assetUseCost a Ammo 1)
    , controlled_ a 2
        $ ForcedWhen (IgnoreModifiersFrom (toSource a) $ not_ $ exists $ CanFightEnemy (a.ability 1))
        $ triggered (ActivateAbility #when You (AbilityIs (toSource a) 1)) (exhaust a)
    ]

instance RunMessage Becky2 where
  runMessage msg (Becky2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let active = getAssetMetaDefault False attrs
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]
      if active
        then chooseFightEnemyMatch sid iid source (ignoreAloofFightOverride AnyEnemy)
        else chooseFightEnemy sid iid source
      pure $ Becky2 $ attrs & setMeta False
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      pure $ Becky2 $ attrs & setMeta True
    _ -> Becky2 <$> liftRunMessage msg attrs
