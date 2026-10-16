module Arkham.Homebrew.TheInvisibleMan.Acts.ZeroVisibility (zeroVisibility) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher

newtype ZeroVisibility = ZeroVisibility ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zeroVisibility :: ActCard ZeroVisibility
zeroVisibility = act (1, A) ZeroVisibility Cards.zeroVisibility Nothing

-- | "Damage cannot be placed on Griffin."
instance HasModifiersFor ZeroVisibility where
  getModifiersFor (ZeroVisibility a) =
    when (onSide A a) $ modifySelect a griffin [CannotBeDamaged]

{- | "If act 1c and act 1e have been advanced, you must immediately advance."
Read off the act decks, which is what the card says: acts 1c and 1e are step 1
of decks 2 and 3, so being off step 1 IS the advance. Griffin's values cannot
stand in for it -- he prints "?", which the engine holds as a value, so
'EnemyWithFight' is true from setup.
-}
instance HasAbilities ZeroVisibility where
  getAbilities = actAbilities1 \x ->
    restricted
      x
      1
      (advancedDeck 2 <> advancedDeck 3)
      $ Objective
      $ forced AnyWindow

{- | "act N has been advanced", i.e. deck @n@ holds an act that is no longer on
step 1. 'ActMatcher' has no conjunction -- only 'ActOneOf' and 'NotAct' -- so
the and is written as @not (not a or b)@. Stays false during setup, while act 1a
is in play but acts 1c and 1e have not been added yet.
-}
advancedDeck :: Int -> Criterion
advancedDeck n = ActExists $ NotAct $ ActOneOf [NotAct (ActWithDeckId n), ActWithStep 1]

instance RunMessage ZeroVisibility where
  runMessage msg a@(ZeroVisibility attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ZeroVisibility <$> liftRunMessage msg attrs
