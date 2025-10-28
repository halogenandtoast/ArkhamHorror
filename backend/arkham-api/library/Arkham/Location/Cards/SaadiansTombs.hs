module Arkham.Location.Cards.SaadiansTombs (saadiansTombs) where

import Arkham.Ability
import Arkham.Helpers.Location (swapLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (getEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Token

newtype SaadiansTombs = SaadiansTombs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saadiansTombs :: LocationCard SaadiansTombs
saadiansTombs = symbolLabel $ location SaadiansTombs Cards.saadiansTombs 1 (PerPlayer 3)

instance HasModifiersFor SaadiansTombs where
  getModifiersFor (SaadiansTombs a) = modifySelf a [ShroudModifier $ a.token Civilian]

instance HasAbilities SaadiansTombs where
  getAbilities (SaadiansTombs a) =
    if a.revealed
      then
        extendRevealed
          a
          [ mkAbility a 1
              $ forced
              $ oneOf [EnemySpawns #when (be a) NonEliteEnemy, EnemyEnters #when (be a) NonEliteEnemy]
          , becomeAbandonedAbility a 2
          ]
      else extendUnrevealed1 a $ becomeAbandonedAbility a 1

instance RunMessage SaadiansTombs where
  runMessage msg l@(SaadiansTombs attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 | attrs.unrevealed -> do
      swapLocation attrs =<< fetchCard Cards.saadiansTombsAbandoned
      pure l
    UseCardAbility _iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      exhaustThis enemy
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      swapLocation attrs =<< fetchCard Cards.saadiansTombsAbandoned
      pure l
    _ -> SaadiansTombs <$> liftRunMessage msg attrs
