module Arkham.Location.Cards.Mu
  ( mu
  , Mu(..)
  ) where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.SkillTest.Base
import Arkham.Target
import Arkham.Token

newtype Mu = Mu LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mu :: LocationCard Mu
mu = location Mu Cards.mu 1 (Static 4)

instance HasModifiersFor Mu where
  getModifiersFor (TokenFaceTarget face) (Mu a) | face `elem` [Skull, Cultist, Tablet, ElderThing] = do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure []
      Just st -> pure
        $ toModifiers a [ RevealAnotherToken | length (skillTestRevealedTokens st) == 1]
  getModifiersFor _ _ = pure []

instance RunMessage Mu where
  runMessage msg (Mu attrs) = Mu <$> runMessage msg attrs
