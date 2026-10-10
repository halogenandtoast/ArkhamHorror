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

{- | "__Forced__ - After you investigate Rome, if you did not succeed by at least
2: Lose an action."

"Did not succeed by at least 2" is a failure /or/ a success with a margin under
2, which is exactly 'ResultOneOf' -- the shape Grand Chamber's identical
proviso uses. The timing is @#after@ here (Grand Chamber prints "when"), so the
clues are already discovered by the time the action is taken away.
-}
instance HasAbilities Rome where
  getAbilities (Rome a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a)
      $ ResultOneOf [#failure, SuccessResult $ LessThan $ Static 2]

instance RunMessage Rome where
  runMessage msg l@(Rome attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseStandardActions iid (attrs.ability 1) 1
      pure l
    _ -> Rome <$> liftRunMessage msg attrs
