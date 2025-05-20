module Arkham.Location.Cards.ReturnToNotreDame (returnToNotreDame) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)

newtype ReturnToNotreDame = ReturnToNotreDame LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToNotreDame :: LocationCard ReturnToNotreDame
returnToNotreDame = location ReturnToNotreDame Cards.returnToNotreDame 4 (PerPlayer 1)

instance HasModifiersFor ReturnToNotreDame where
  getModifiersFor (ReturnToNotreDame a) = do
    modifySelect a (EnemyAt $ be a) [EnemyFight 1, EnemyEvade (-1)]

instance HasAbilities ReturnToNotreDame where
  getAbilities (ReturnToNotreDame a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (DuringTurn You)
      $ freeReaction
      $ SkillTestResult #after You (WhileEvadingAnEnemy AnyEnemy) (SuccessResult $ atLeast 3)

instance RunMessage ReturnToNotreDame where
  runMessage msg l@(ReturnToNotreDame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainActions iid (attrs.ability 1) 1
      pure l
    _ -> ReturnToNotreDame <$> liftRunMessage msg attrs
