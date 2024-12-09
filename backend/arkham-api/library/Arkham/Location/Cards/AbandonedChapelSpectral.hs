module Arkham.Location.Cards.AbandonedChapelSpectral (abandonedChapelSpectral, AbandonedChapelSpectral (..)) where

import Arkham.Card
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Phase
import Arkham.Prelude
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
    withRevealedAbilities a
      $ [haunted "Until the end of the round, you get -1 to each skill." a 1]

instance RunMessage AbandonedChapelSpectral where
  runMessage msg l@(AbandonedChapelSpectral attrs) = case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      regular <- genCard Locations.abandonedChapel
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    _ -> AbandonedChapelSpectral <$> runMessage msg attrs
