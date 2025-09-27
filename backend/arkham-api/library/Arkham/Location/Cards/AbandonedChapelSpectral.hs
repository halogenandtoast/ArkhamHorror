module Arkham.Location.Cards.AbandonedChapelSpectral (abandonedChapelSpectral) where

import Arkham.Ability
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.SkillType

newtype AbandonedChapelSpectral = AbandonedChapelSpectral LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedChapelSpectral :: LocationCard AbandonedChapelSpectral
abandonedChapelSpectral = location AbandonedChapelSpectral Cards.abandonedChapelSpectral 2 (Static 0)

-- During the mythos phase, each investigator in Abandoned Chapel gets -1 to each skill.
instance HasModifiersFor AbandonedChapelSpectral where
  getModifiersFor (AbandonedChapelSpectral a) = do
    phase <- getPhase
    modifySelectWhen
      a
      (isMythosPhase phase)
      (investigatorAt a)
      [SkillModifier sType (-1) | sType <- allSkills]

instance HasAbilities AbandonedChapelSpectral where
  getAbilities (AbandonedChapelSpectral a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "abandonedChapelSpectral.haunted" a 1

instance RunMessage AbandonedChapelSpectral where
  runMessage msg l@(AbandonedChapelSpectral attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.abandonedChapel
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifiers (attrs.ability 1) iid [SkillModifier sType (-1) | sType <- allSkills]
      pure l
    _ -> AbandonedChapelSpectral <$> liftRunMessage msg attrs
