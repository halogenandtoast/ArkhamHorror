module Arkham.Location.Cards.RehearsalRoom (rehearsalRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RehearsalRoom = RehearsalRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rehearsalRoom :: LocationCard RehearsalRoom
rehearsalRoom = location RehearsalRoom Cards.rehearsalRoom 1 (PerPlayer 1)

instance HasAbilities RehearsalRoom where
  getAbilities (RehearsalRoom a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) (SuccessResult $ AtLeast $ Static 2)

instance RunMessage RehearsalRoom where
  runMessage msg l@(RehearsalRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> RehearsalRoom <$> liftRunMessage msg attrs
