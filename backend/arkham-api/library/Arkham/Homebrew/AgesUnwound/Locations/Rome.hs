module Arkham.Homebrew.AgesUnwound.Locations.Rome (rome) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Rome = Rome LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rome :: LocationCard Rome
rome = symbolLabel $ location Rome Cards.rome 2 (PerPlayer 2)

instance HasAbilities Rome where
  getAbilities (Rome a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult
        #after
        (You <> InvestigatorWithActionsRemaining (atLeast 1))
        (WhileInvestigating $ be a)
      $ ResultOneOf [#failure, SuccessResult $ LessThan $ Static 2]

instance RunMessage Rome where
  runMessage msg l@(Rome attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    _ -> Rome <$> liftRunMessage msg attrs
