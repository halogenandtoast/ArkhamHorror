module Arkham.Treachery.Cards.IncriminatingEvidence (incriminatingEvidence) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (CrimeScene))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IncriminatingEvidence = IncriminatingEvidence TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

incriminatingEvidence :: TreacheryCard IncriminatingEvidence
incriminatingEvidence = treachery IncriminatingEvidence Cards.incriminatingEvidence

instance HasModifiersFor IncriminatingEvidence where
  getModifiersFor (IncriminatingEvidence a) = case a.placement of
    AttachedToLocation lid -> modified_ a lid [AddTrait CrimeScene, ShroudModifier 2]
    _ -> pure mempty

instance HasAbilities IncriminatingEvidence where
  getAbilities (IncriminatingEvidence attrs) = case attrs.attached.location of
    Just lid ->
      [ mkAbility attrs 1
          $ freeReaction
          $ SkillTestResult #when You (WhileInvestigating $ LocationWithId lid)
          $ SuccessResult AnyValue
      ]
    _ -> []

instance RunMessage IncriminatingEvidence where
  runMessage msg t@(IncriminatingEvidence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      nonCrimeScenes <- select $ NearestLocationTo iid $ NotLocation $ LocationWithTrait CrimeScene
      chooseOrRunOneM iid $ targets nonCrimeScenes $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case attrs.attached.location of
        Just lid -> withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) lid (AlternateSuccessfullInvestigation $ toTarget attrs)
        _ -> error "Unexpected"
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> IncriminatingEvidence <$> liftRunMessage msg attrs
