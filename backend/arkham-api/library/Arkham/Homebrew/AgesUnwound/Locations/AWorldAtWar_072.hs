module Arkham.Homebrew.AgesUnwound.Locations.AWorldAtWar_072 (aWorldAtWar_072) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype AWorldAtWar_072 = AWorldAtWar_072 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /A World at War/, the printing that bleeds clues onto the battlefield.
aWorldAtWar_072 :: LocationCard AWorldAtWar_072
aWorldAtWar_072 =
  locationWith AWorldAtWar_072 Cards.aWorldAtWar_072 3 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [willpower] (3). If you fail, place
1 of your clues on this location."
-}
instance HasAbilities AWorldAtWar_072 where
  getAbilities (AWorldAtWar_072 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage AWorldAtWar_072 where
  runMessage msg l@(AWorldAtWar_072 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "1 of your clues", so the clue moves from the investigator rather than
      -- being minted onto the location.
      moveTokens (attrs.ability 1) iid attrs Token.Clue 1
      pure l
    _ -> AWorldAtWar_072 <$> liftRunMessage msg attrs
