module Arkham.Location.Cards.Mu (mu, Mu (..)) where

import Arkham.ChaosToken
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Location (isAt)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.SkillTest.Base

newtype Mu = Mu LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mu :: LocationCard Mu
mu = location Mu Cards.mu 1 (Static 4)

instance HasModifiersFor Mu where
  getModifiersFor (Mu a) =
    fromMaybe mempty <$> runMaybeT do
      iid <- MaybeT getSkillTestInvestigator
      liftGuardM $ iid `isAt` a
      st <- MaybeT getSkillTest
      guard $ length (skillTestRevealedChaosTokens st) == 1
      lift
        $ modifyEach
          a
          (map ChaosTokenFaceTarget [Skull, Cultist, Tablet, ElderThing])
          [RevealAnotherChaosToken]

instance RunMessage Mu where
  runMessage msg (Mu attrs) = Mu <$> runMessage msg attrs
