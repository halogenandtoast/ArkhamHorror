module Arkham.Location.Cards.Mu (mu, Mu (..)) where

import Arkham.ChaosToken
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.SkillTest.Base

newtype Mu = Mu LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mu :: LocationCard Mu
mu = location Mu Cards.mu 1 (Static 4)

instance HasModifiersFor Mu where
  getModifiersFor (ChaosTokenFaceTarget face) (Mu a) = maybeModified a do
    guard $ face `elem` [Skull, Cultist, Tablet, ElderThing]
    st <- MaybeT getSkillTest
    guard $ length (skillTestRevealedChaosTokens st) == 1
    pure [RevealAnotherChaosToken]
  getModifiersFor _ _ = pure []

instance RunMessage Mu where
  runMessage msg (Mu attrs) = Mu <$> runMessage msg attrs
