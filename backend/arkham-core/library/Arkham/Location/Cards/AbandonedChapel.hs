module Arkham.Location.Cards.AbandonedChapel (abandonedChapel, AbandonedChapel (..)) where

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

newtype AbandonedChapel = AbandonedChapel LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abandonedChapel :: LocationCard AbandonedChapel
abandonedChapel = location AbandonedChapel Cards.abandonedChapel 2 (PerPlayer 2)

-- During the mythos phase, each investigator in Abandoned Chapel gets -1 to each skill.
instance HasModifiersFor AbandonedChapel where
  getModifiersFor (InvestigatorTarget iid) (AbandonedChapel a) = do
    here <- iid <=~> investigatorAt (toId a)
    phase <- getPhase
    pure $ toModifiers a [SkillModifier sType (-1) | here, isMythosPhase phase, sType <- allSkills]
  getModifiersFor _ _ = pure []

instance RunMessage AbandonedChapel where
  runMessage msg l@(AbandonedChapel attrs) = case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      spectral <- genCard Locations.abandonedChapelSpectral
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> AbandonedChapel <$> runMessage msg attrs
