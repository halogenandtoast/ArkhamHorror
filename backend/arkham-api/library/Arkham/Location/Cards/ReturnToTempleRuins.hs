module Arkham.Location.Cards.ReturnToTempleRuins (returnToTempleRuins) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToTempleRuins = ReturnToTempleRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToTempleRuins :: LocationCard ReturnToTempleRuins
returnToTempleRuins = symbolLabel $ location ReturnToTempleRuins Cards.returnToTempleRuins 5 (Static 0)

instance HasAbilities ReturnToTempleRuins where
  getAbilities (ReturnToTempleRuins a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ mkAbility a 1 (freeReaction $ SkillTestResult #after You (whileInvestigating a) #success)

instance RunMessage ReturnToTempleRuins where
  runMessage msg l@(ReturnToTempleRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ReturnToTempleRuins <$> liftRunMessage msg attrs
