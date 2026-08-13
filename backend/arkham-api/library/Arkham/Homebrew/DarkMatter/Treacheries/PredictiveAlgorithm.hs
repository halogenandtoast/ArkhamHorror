module Arkham.Homebrew.DarkMatter.Treacheries.PredictiveAlgorithm (predictiveAlgorithm) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype PredictiveAlgorithm = PredictiveAlgorithm TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predictiveAlgorithm :: TreacheryCard PredictiveAlgorithm
predictiveAlgorithm = treachery PredictiveAlgorithm Cards.predictiveAlgorithm

instance HasModifiersFor PredictiveAlgorithm where
  getModifiersFor (PredictiveAlgorithm a) = case a.placement of
    InThreatArea iid -> runMaybeT_ do
      action <- MaybeT getSkillTestAction
      guard $ action `elem` [#fight, #evade]
      enemy <- MaybeT getSkillTestTargetedEnemy
      liftGuardM $ enemy <=~> EnemyWithTrait AI
      lift $ modifySelect a (InvestigatorWithId iid) [SkillModifier sk (-1) | sk <- [minBound ..]]
    _ -> pure ()

instance HasAbilities PredictiveAlgorithm where
  getAbilities (PredictiveAlgorithm a) =
    [restricted a 1 (InThreatAreaOf You) $ actionAbilityWithCost (HandDiscardCost 2 #any)]

instance RunMessage PredictiveAlgorithm where
  runMessage msg t@(PredictiveAlgorithm attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PredictiveAlgorithm <$> liftRunMessage msg attrs
