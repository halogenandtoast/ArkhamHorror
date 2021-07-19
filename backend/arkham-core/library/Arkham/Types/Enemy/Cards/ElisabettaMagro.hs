module Arkham.Types.Enemy.Cards.ElisabettaMagro
  ( elisabettaMagro
  , ElisabettaMagro(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Message

newtype ElisabettaMagro = ElisabettaMagro EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elisabettaMagro :: EnemyCard ElisabettaMagro
elisabettaMagro =
  enemy ElisabettaMagro Cards.elisabettaMagro (3, Static 4, 4) (1, 1)

instance HasModifiersFor env ElisabettaMagro

instance EnemyAttrsHasActions env => HasActions env ElisabettaMagro where
  getActions i window (ElisabettaMagro attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env ElisabettaMagro where
  runMessage msg (ElisabettaMagro attrs) = case msg of
    EndMythos -> pure $ ElisabettaMagro $ attrs & doomL +~ 1
    _ -> ElisabettaMagro <$> runMessage msg attrs
