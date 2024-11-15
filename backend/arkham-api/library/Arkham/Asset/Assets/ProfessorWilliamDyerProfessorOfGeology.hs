module Arkham.Asset.Assets.ProfessorWilliamDyerProfessorOfGeology (
  professorWilliamDyerProfessorOfGeology,
  ProfessorWilliamDyerProfessorOfGeology (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ProfessorWilliamDyerProfessorOfGeology = ProfessorWilliamDyerProfessorOfGeology AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWilliamDyerProfessorOfGeology :: AssetCard ProfessorWilliamDyerProfessorOfGeology
professorWilliamDyerProfessorOfGeology =
  allyWith
    ProfessorWilliamDyerProfessorOfGeology
    Cards.professorWilliamDyerProfessorOfGeology
    (1, 5)
    noSlots

instance RunMessage ProfessorWilliamDyerProfessorOfGeology where
  runMessage msg (ProfessorWilliamDyerProfessorOfGeology attrs) = runQueueT $ case msg of
    _ -> ProfessorWilliamDyerProfessorOfGeology <$> liftRunMessage msg attrs
