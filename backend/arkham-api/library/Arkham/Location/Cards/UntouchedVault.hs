module Arkham.Location.Cards.UntouchedVault (untouchedVault) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.History.Types
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Monster))

newtype UntouchedVault = UntouchedVault LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

untouchedVault :: LocationCard UntouchedVault
untouchedVault = symbolLabel $ location UntouchedVault Cards.untouchedVault 4 (PerPlayer 1)

instance HasModifiersFor UntouchedVault where
  getModifiersFor (UntouchedVault a) = do
    modifySelect a (EnemyWithTrait Monster <> enemyAt a) [EnemyFight 2, DamageDealt 1]

instance HasAbilities UntouchedVault where
  getAbilities (UntouchedVault a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> HasHistory RoundHistory Anyone (DefeatedEnemyWithTraitAt Monster a.id)) actionAbility

instance RunMessage UntouchedVault where
  runMessage msg l@(UntouchedVault attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeStrengthOfTheAbyss 1
      pure l
    _ -> UntouchedVault <$> liftRunMessage msg attrs
