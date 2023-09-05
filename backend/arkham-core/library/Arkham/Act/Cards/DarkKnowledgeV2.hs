module Arkham.Act.Cards.DarkKnowledgeV2
  ( DarkKnowledgeV2(..)
  , darkKnowledgeV2
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype DarkKnowledgeV2 = DarkKnowledgeV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

darkKnowledgeV2 :: ActCard DarkKnowledgeV2
darkKnowledgeV2 = act (1, A) DarkKnowledgeV2 Cards.darkKnowledgeV2 Nothing

instance RunMessage DarkKnowledgeV2 where
  runMessage msg (DarkKnowledgeV2 attrs) = DarkKnowledgeV2 <$> runMessage msg attrs
