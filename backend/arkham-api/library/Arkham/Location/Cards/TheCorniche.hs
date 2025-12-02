module Arkham.Location.Cards.TheCorniche (theCorniche, theCornicheEffect) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheCorniche = TheCorniche LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCorniche :: LocationCard TheCorniche
theCorniche = symbolLabel $ location TheCorniche Cards.theCorniche 2 (PerPlayer 1)

instance HasAbilities TheCorniche where
  getAbilities (TheCorniche a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #success

instance RunMessage TheCorniche where
  runMessage msg l@(TheCorniche attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      createCardEffect Cards.theCorniche Nothing (attrs.ability 1) attrs
      pure l
    _ -> TheCorniche <$> liftRunMessage msg attrs

newtype TheCornicheEffect = TheCornicheEffect EffectAttrs
  deriving anyclass (IsEffect, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCornicheEffect :: EffectArgs -> TheCornicheEffect
theCornicheEffect = cardEffect TheCornicheEffect Cards.theCorniche

instance HasModifiersFor TheCornicheEffect where
  getModifiersFor (TheCornicheEffect a) = do
    whenJustM getSkillTest \st ->
      withLocationOf st.investigator \loc ->
        modifiedWhen_ a (isTarget loc a.target) st [Difficulty 2]

instance RunMessage TheCornicheEffect where
  runMessage msg e@(TheCornicheEffect attrs) = runQueueT $ case msg of
    EndRound -> disableReturn e
    _ -> TheCornicheEffect <$> liftRunMessage msg attrs
