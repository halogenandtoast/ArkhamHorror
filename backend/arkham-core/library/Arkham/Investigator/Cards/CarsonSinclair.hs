module Arkham.Investigator.Cards.CarsonSinclair (carsonSinclair, CarsonSinclair (..)) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype CarsonSinclair = CarsonSinclair InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carsonSinclair :: InvestigatorCard CarsonSinclair
carsonSinclair =
  investigator CarsonSinclair Cards.carsonSinclair
    $ Stats {health = 6, sanity = 6, willpower = 2, intellect = 2, combat = 2, agility = 2}

instance HasModifiersFor CarsonSinclair where
  getModifiersFor target (CarsonSinclair a) | a `is` target = do
    modified
      a
      [ GiveAdditionalAction
          $ AdditionalAction "Carson Sinclair" (toSource a)
          $ AbilityRestrictedAdditionalAction (toSource a) 1
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities CarsonSinclair where
  getAbilities (CarsonSinclair a) =
    [ restrictedAbility
        a
        1
        (Self <> exists (affectsOthers $ InvestigatorAt YourLocation <> notOneOf invalid))
        actionAbility
    ]
   where
    invalid = You : (InvestigatorWithId <$> lookupMetaKeyWithDefault "used" [] a)

instance HasChaosTokenValue CarsonSinclair where
  getChaosTokenValue iid ElderSign (CarsonSinclair attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CarsonSinclair where
  runMessage msg i@(CarsonSinclair attrs) = runQueueT $ case msg of
    BeginTurn iid | iid == attrs.id -> do
      pure . CarsonSinclair $ attrs & insertMetaKey "wasNotSelfless"
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let invalid = InvestigatorWithId <$> (iid : lookupMetaKeyWithDefault "used" [] attrs)
      investigators <- select $ affectsOthers $ colocatedWith iid <> notOneOf invalid

      when (notNull investigators) do
        chooseOne iid $ targetLabels investigators $ only . handleTargetChoice iid (attrs.ability 1)
      pure i
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      push $ PlayerWindow iid' [] True
      pure . CarsonSinclair $ attrs & overMetaKey "used" (<>) [iid']
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      drawCardsIfCan attrs.id attrs 1
      pure i
    ResolveChaosToken t ElderSign iid | not (attrs `is` iid) -> do
      whenM (iid <=~> colocatedWith attrs.id) do
        chooseOne
          attrs.id
          [ Label "Resolve your Elder Sign ability" [ResolveChaosToken t ElderSign attrs.id]
          , Label "Do not resolve your Elder Sign ability" []
          ]
      pure i
    EndRound -> CarsonSinclair <$> liftRunMessage msg (attrs & deleteMetaKey "used")
    InvestigatorCommittedCard iid _ | attrs `is` iid -> do
      attrs' <- liftRunMessage msg attrs
      isTurn <- iid <=~> TurnInvestigator
      getSkillTestInvestigator >>= \case
        Just iid' | isTurn, iid' /= iid -> pure . CarsonSinclair $ attrs' & deleteMetaKey "wasNotSelfless"
        _ -> pure $ CarsonSinclair attrs'
    InvestigatorCommittedSkill iid _ | attrs `is` iid -> do
      attrs' <- liftRunMessage msg attrs
      isTurn <- iid <=~> TurnInvestigator
      getSkillTestInvestigator >>= \case
        Just iid' | isTurn, iid' /= iid -> pure . CarsonSinclair $ attrs' & deleteMetaKey "wasNotSelfless"
        _ -> pure $ CarsonSinclair attrs'
    _ -> CarsonSinclair <$> liftRunMessage msg attrs
