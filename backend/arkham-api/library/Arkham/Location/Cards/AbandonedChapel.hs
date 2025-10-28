module Arkham.Location.Cards.AbandonedChapel (abandonedChapel) where

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
import Arkham.SkillType

newtype AbandonedChapel = AbandonedChapel LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abandonedChapel :: LocationCard AbandonedChapel
abandonedChapel = location AbandonedChapel Cards.abandonedChapel 2 (PerPlayer 2)

-- During the mythos phase, each investigator in Abandoned Chapel gets -1 to each skill.
instance HasModifiersFor AbandonedChapel where
  getModifiersFor (AbandonedChapel a) = do
    phase <- getPhase
    modifySelectWhen
      a
      (isMythosPhase phase)
      (investigatorAt a)
      [SkillModifier sType (-1) | sType <- allSkills]

instance RunMessage AbandonedChapel where
  runMessage msg l@(AbandonedChapel attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.abandonedChapelSpectral
      pure l
    _ -> AbandonedChapel <$> liftRunMessage msg attrs
