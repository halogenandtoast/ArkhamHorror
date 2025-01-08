module Arkham.Asset.Assets.EyesOfValusiaTheMothersCunning4 (
  eyesOfValusiaTheMothersCunning4,
  eyesOfValusiaTheMothersCunning4Effect,
  EyesOfValusiaTheMothersCunning4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Token

newtype EyesOfValusiaTheMothersCunning4 = EyesOfValusiaTheMothersCunning4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesOfValusiaTheMothersCunning4 :: AssetCard EyesOfValusiaTheMothersCunning4
eyesOfValusiaTheMothersCunning4 = asset EyesOfValusiaTheMothersCunning4 Cards.eyesOfValusiaTheMothersCunning4

instance HasAbilities EyesOfValusiaTheMothersCunning4 where
  getAbilities (EyesOfValusiaTheMothersCunning4 a) =
    [ controlledAbility a 1 (exists $ EnemyAt YourLocation) parleyAction_
    , restrictedAbility a 2 ControlsThis $ FastAbility Free
    ]

instance RunMessage EyesOfValusiaTheMothersCunning4 where
  runMessage msg a@(EyesOfValusiaTheMothersCunning4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1) $ enemyAtLocationWith iid
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (EnemyTarget eid) -> do
      createCardEffect Cards.eyesOfValusiaTheMothersCunning4 Nothing (attrs.ability 1) eid
      placeTokens (attrs.ability 1) attrs Charge 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      bladeOfYoth <- searchBondedJust iid Cards.bladeOfYothTheFathersIre
      push $ ReplaceInvestigatorAsset iid attrs.id bladeOfYoth
      placeInBonded iid attrs
      pure a
    _ -> EyesOfValusiaTheMothersCunning4 <$> liftRunMessage msg attrs

newtype EyesOfValusiaTheMothersCunning4Effect
  = EyesOfValusiaTheMothersCunning4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor EyesOfValusiaTheMothersCunning4Effect where
  getModifiersFor (EyesOfValusiaTheMothersCunning4Effect a) =
    modifySelectMaybe a Anyone \iid -> do
      st <- MaybeT getSkillTest
      guard $ iid == st.investigator
      guard $ any (`elem` st.action) [#fight, #evade, #parley]
      eid <- hoistMaybe st.target.enemy
      guard $ isTarget eid a.target
      pure [AnySkillValue 1]

eyesOfValusiaTheMothersCunning4Effect
  :: EffectArgs -> EyesOfValusiaTheMothersCunning4Effect
eyesOfValusiaTheMothersCunning4Effect =
  cardEffect
    EyesOfValusiaTheMothersCunning4Effect
    Cards.eyesOfValusiaTheMothersCunning4

instance RunMessage EyesOfValusiaTheMothersCunning4Effect where
  runMessage msg e@(EyesOfValusiaTheMothersCunning4Effect attrs) = runQueueT $ case msg of
    EndRound -> disableReturn e
    _ -> EyesOfValusiaTheMothersCunning4Effect <$> liftRunMessage msg attrs
