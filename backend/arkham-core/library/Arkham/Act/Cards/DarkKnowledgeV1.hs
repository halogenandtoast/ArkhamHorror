module Arkham.Act.Cards.DarkKnowledgeV1
  ( DarkKnowledgeV1(..)
  , darkKnowledgeV1
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype DarkKnowledgeV1 = DarkKnowledgeV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

darkKnowledgeV1 :: ActCard DarkKnowledgeV1
darkKnowledgeV1 = act (1, A) DarkKnowledgeV1 Cards.darkKnowledgeV1 Nothing

instance RunMessage DarkKnowledgeV1 where
  runMessage msg (DarkKnowledgeV1 attrs) = DarkKnowledgeV1 <$> runMessage msg attrs
