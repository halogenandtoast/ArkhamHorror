module Arkham.Investigator.Cards.LostHomunculus (lostHomunculus, LostHomunculus (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModifySelf, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTestSource, isSkillTestInvestigator)
import Arkham.Helpers.Source
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Investigator.Types (InvestigatorForm (..))
import Arkham.Matcher

newtype LostHomunculus = LostHomunculus InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

lostHomunculus :: InvestigatorCard LostHomunculus
lostHomunculus =
  investigatorWith
    LostHomunculus
    Cards.lostHomunculus
    (Stats {health = 6, sanity = 6, willpower = 2, intellect = 2, combat = 2, agility = 2})
    (formL .~ HomunculusForm)

instance HasModifiersFor LostHomunculus where
  getModifiersFor (LostHomunculus a) = do
    modifySelf a [KilledIfDefeated]
    maybeModifySelf a do
      liftGuardM $ isSkillTestInvestigator a.id
      source <- MaybeT getSkillTestSource
      liftGuardM $ sourceMatches source #spell
      pure [AnySkillValue 2]

instance HasAbilities LostHomunculus where
  getAbilities (LostHomunculus a) =
    [restricted a 1 Self $ forced $ InvestigatorDefeated #when ByAny You]

instance HasChaosTokenValue LostHomunculus where
  getChaosTokenValue iid ElderSign (LostHomunculus attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign ZeroModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage LostHomunculus where
  runMessage msg i@(LostHomunculus attrs) = runQueueT $ case msg of
    ElderSignEffect (is attrs -> True) -> do
      drawCards attrs.id ElderSign 1
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      kill (attrs.ability 1) iid
      pure i
    _ -> LostHomunculus <$> liftRunMessage msg attrs
