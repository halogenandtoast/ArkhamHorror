module Arkham.Enemy.Cards.TheForgottenAge.ThreadsOfFate.HenryDeveauAlejandrosKidnapper (henryDeveauAlejandrosKidnapper) where

import Arkham.Enemy.CardDefs.TheForgottenAge.ThreadsOfFate qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HenryDeveauAlejandrosKidnapper = HenryDeveauAlejandrosKidnapper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

henryDeveauAlejandrosKidnapper :: EnemyCard HenryDeveauAlejandrosKidnapper
henryDeveauAlejandrosKidnapper =
  enemy HenryDeveauAlejandrosKidnapper Cards.henryDeveauAlejandrosKidnapper

instance RunMessage HenryDeveauAlejandrosKidnapper where
  runMessage msg (HenryDeveauAlejandrosKidnapper attrs) =
    HenryDeveauAlejandrosKidnapper <$> runMessage msg attrs
