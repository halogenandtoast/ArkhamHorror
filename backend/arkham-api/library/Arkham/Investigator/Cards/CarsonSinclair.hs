module Arkham.Investigator.Cards.CarsonSinclair (carsonSinclair, carsonSinclairEffect) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CarsonSinclair = CarsonSinclair InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

carsonSinclair :: InvestigatorCard CarsonSinclair
carsonSinclair =
  investigator CarsonSinclair Cards.carsonSinclair
    $ Stats {health = 6, sanity = 6, willpower = 2, intellect = 2, combat = 2, agility = 2}

instance HasModifiersFor CarsonSinclair where
  getModifiersFor (CarsonSinclair a) = modifySelf a [GiveAdditionalAction additionalAction]
   where
    additionalAction =
      AdditionalAction "Carson Sinclair" (toSource a)
        $ AbilityRestrictedAdditionalAction (toSource a) 1

instance HasAbilities CarsonSinclair where
  getAbilities (CarsonSinclair a) =
    [selfAbility a 1 (exists (affectsOthers $ at_ YourLocation <> notOneOf invalid)) actionAbility]
   where
    invalid = You : (InvestigatorWithId <$> lookupMetaKeyWithDefault "used" [] a)

instance HasChaosTokenValue CarsonSinclair where
  getChaosTokenValue iid ElderSign (CarsonSinclair attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CarsonSinclair where
  runMessage msg i@(CarsonSinclair attrs) = runQueueT $ case msg of
    BeginGame -> do
      createCardEffect Cards.carsonSinclair Nothing attrs attrs
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let invalid = InvestigatorWithId <$> (iid : lookupMetaKeyWithDefault "used" [] attrs)
      investigators <- select $ affectsOthers $ colocatedWith iid <> notOneOf invalid
      chooseTargetM iid investigators $ handleTarget iid (attrs.ability 1)
      pure i
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      takeActionAsIfTurn iid' (attrs.ability 1)
      pure . CarsonSinclair $ attrs & overMetaKey "used" (<>) [iid']
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      drawCardsIfCan attrs.id attrs 1
      pure i
    ResolveChaosToken t ElderSign iid | not (attrs `is` iid) -> do
      whenM (iid <=~> colocatedWith attrs.id) do
        chooseOneM attrs.id do
          labeled "Resolve your Elder Sign ability" $ push $ ResolveChaosToken t ElderSign attrs.id
          labeled "Do not resolve your Elder Sign ability" nothing
      pure i
    EndRound -> CarsonSinclair <$> liftRunMessage msg (attrs & deleteMetaKey "used")
    _ -> CarsonSinclair <$> liftRunMessage msg attrs

newtype CarsonSinclairEffect = CarsonSinclairEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carsonSinclairEffect :: EffectArgs -> CarsonSinclairEffect
carsonSinclairEffect = cardEffectWith CarsonSinclairEffect Cards.carsonSinclair (setEffectMeta False)

instance HasModifiersFor CarsonSinclairEffect where
  getModifiersFor (CarsonSinclairEffect a) = for_ a.target.investigator \iid -> do
    modifiedWhen_ a (hasEffectKey "wasNotSelfless" a) iid [MetaModifier "wasNotSelfless"]

instance RunMessage CarsonSinclairEffect where
  runMessage msg e@(CarsonSinclairEffect attrs) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      pure . CarsonSinclairEffect $ attrs & setEffectKey "wasNotSelfless"
    InvestigatorCommittedCard iid _ | isTarget iid attrs.target -> do
      isTurn <- iid <=~> TurnInvestigator
      getSkillTestInvestigator >>= \case
        Just iid' | isTurn, iid' /= iid -> pure . CarsonSinclairEffect $ attrs & unsetEffectKey "wasNotSelfless"
        _ -> pure e
    InvestigatorCommittedSkill iid _ | isTarget iid attrs.target -> do
      isTurn <- iid <=~> TurnInvestigator
      getSkillTestInvestigator >>= \case
        Just iid' | isTurn, iid' /= iid -> pure . CarsonSinclairEffect $ attrs & unsetEffectKey "wasNotSelfless"
        _ -> pure e
    _ -> CarsonSinclairEffect <$> liftRunMessage msg attrs
