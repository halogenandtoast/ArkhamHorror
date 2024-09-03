module Arkham.Asset.Cards.BladeOfYothTheFathersIre (
  bladeOfYothTheFathersIre,
  BladeOfYothTheFathersIre (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Token

newtype BladeOfYothTheFathersIre = BladeOfYothTheFathersIre AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bladeOfYothTheFathersIre :: AssetCard BladeOfYothTheFathersIre
bladeOfYothTheFathersIre = asset BladeOfYothTheFathersIre Cards.bladeOfYothTheFathersIre

instance HasAbilities BladeOfYothTheFathersIre where
  getAbilities (BladeOfYothTheFathersIre a) =
    [ restrictedAbility a 1 ControlsThis $ fightAction $ UseCostUpTo (be a) Charge 1 3
    , restrictedAbility a 2 ControlsThis $ FastAbility Free
    ]

instance RunMessage BladeOfYothTheFathersIre where
  runMessage msg a@(BladeOfYothTheFathersIre attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [AnySkillValue 2, DamageDealt n]
      chooseFightEnemyWithSkillChoice sid iid (attrs.ability 1) [#willpower, #combat]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      eyesOfValusia <- searchBondedJust iid Cards.eyesOfValusiaTheMothersCunning4
      push $ ReplaceInvestigatorAsset iid attrs.id eyesOfValusia
      placeInBonded iid attrs
      pure . BladeOfYothTheFathersIre $ attrs & tokensL . at Charge ?~ 0
    _ -> BladeOfYothTheFathersIre <$> liftRunMessage msg attrs
