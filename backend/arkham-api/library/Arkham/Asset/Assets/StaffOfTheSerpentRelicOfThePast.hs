module Arkham.Asset.Assets.StaffOfTheSerpentRelicOfThePast (staffOfTheSerpentRelicOfThePast) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Elite))

newtype StaffOfTheSerpentRelicOfThePast = StaffOfTheSerpentRelicOfThePast AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

staffOfTheSerpentRelicOfThePast :: AssetCard StaffOfTheSerpentRelicOfThePast
staffOfTheSerpentRelicOfThePast = assetWith StaffOfTheSerpentRelicOfThePast Cards.staffOfTheSerpentRelicOfThePast (healthL ?~ 6)

instance HasModifiersFor StaffOfTheSerpentRelicOfThePast where
  getModifiersFor (StaffOfTheSerpentRelicOfThePast a) =
    modifySelf a [CannotBeDamagedBySourcesExcept $ SourceIs $ AbilitySource (toSource a) 1]

instance HasAbilities StaffOfTheSerpentRelicOfThePast where
  getAbilities (StaffOfTheSerpentRelicOfThePast a) =
    [ controlled a 1 (ActExists $ ActWithStep 3) fightAction_
    , playerLimit PerRound $ fastAbility a 2 Free (exists $ colocatedWithMatch You <> not_ You)
    , restricted a 3 (exists $ not_ You)
        $ SilentForcedAbility
        $ InvestigatorDefeated #when ByAny (ControlsAsset $ be a)
    ]

instance RunMessage StaffOfTheSerpentRelicOfThePast where
  runMessage msg a@(StaffOfTheSerpentRelicOfThePast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (AddSkillValue #willpower)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      others <- select $ colocatedWith iid <> not_ (InvestigatorWithId iid)
      chooseOrRunOneM iid $ targets others (`takeControlOfAsset` attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      for_ attrs.controller \controller -> do
        others <- select $ not_ (be controller) <> NearestToLocation (locationWithInvestigator controller)
        chooseOrRunOneM iid $ targets others (`takeControlOfAsset` attrs)
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) traits -> do
      dealAssetDamage attrs.id (attrs.ability 1) $ if Elite `elem` traits then 2 else 1
      pure a
    _ -> StaffOfTheSerpentRelicOfThePast <$> liftRunMessage msg attrs
