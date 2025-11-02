module Arkham.Location.Cards.ObeliskOfTheodosius (obeliskOfTheodosius) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ObeliskOfTheodosius = ObeliskOfTheodosius LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obeliskOfTheodosius :: LocationCard ObeliskOfTheodosius
obeliskOfTheodosius = symbolLabel $ location ObeliskOfTheodosius Cards.obeliskOfTheodosius 3 (PerPlayer 1)

instance HasAbilities ObeliskOfTheodosius where
  getAbilities (ObeliskOfTheodosius a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist InvestigatorWithAnyClues <> exists (enemy_ #cultist))
      $ forced
      $ SkillTestResult #after You (WhileInvestigating (be a)) #failure

instance RunMessage ObeliskOfTheodosius where
  runMessage msg l@(ObeliskOfTheodosius attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cultists <- select $ NearestEnemyToFallback iid #cultist
      chooseTargetM iid cultists $ moveTokensTo (attrs.ability 1) iid #clue 1
      pure l
    _ -> ObeliskOfTheodosius <$> liftRunMessage msg attrs
