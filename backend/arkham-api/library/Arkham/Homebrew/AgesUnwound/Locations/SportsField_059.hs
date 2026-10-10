module Arkham.Homebrew.AgesUnwound.Locations.SportsField_059 (sportsField_059) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SportsField_059 = SportsField_059 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sportsField_059 :: LocationCard SportsField_059
sportsField_059 = symbolLabel $ location SportsField_059 Cards.sportsField_059 2 (PerPlayer 1)

{- | "[reaction] After you succeed at a skill test by 2 or more while
investigating Sports Field: Heal 1 horror. (Limit once per round.)"
-}
instance HasAbilities SportsField_059 where
  getAbilities (SportsField_059 a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (exists $ HealableInvestigator (a.ability 1) #horror You)
      $ freeReaction
      $ SuccessfulInvestigationResult #after You (be a) (atLeast 2)

instance RunMessage SportsField_059 where
  runMessage msg l@(SportsField_059 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 1
      pure l
    _ -> SportsField_059 <$> liftRunMessage msg attrs
