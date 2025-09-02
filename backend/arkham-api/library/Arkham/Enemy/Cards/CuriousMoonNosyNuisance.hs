module Arkham.Enemy.Cards.CuriousMoonNosyNuisance (curiousMoonNosyNuisance) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Placement

newtype CuriousMoonNosyNuisance = CuriousMoonNosyNuisance EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curiousMoonNosyNuisance :: EnemyCard CuriousMoonNosyNuisance
curiousMoonNosyNuisance =
  enemyWith
    CuriousMoonNosyNuisance
    Cards.curiousMoonNosyNuisance
    (4, Static 1, 4)
    (2, 0)
    (healthL .~ Nothing)

instance HasAbilities CuriousMoonNosyNuisance where
  getAbilities (CuriousMoonNosyNuisance a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ oneOf [EnemyEvaded #after Anyone (be a), EnemyAttackedSuccessfully #after Anyone AnySource (be a)]

instance RunMessage CuriousMoonNosyNuisance where
  runMessage msg e@(CuriousMoonNosyNuisance attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      withLocationOf attrs \lid -> do
        let card = lookupCard Assets.rocketShipRattlingWithEnergy (toCardId attrs)
        createAssetAt_ card (AtLocation lid)
        push $ Flipped (toSource attrs) card
      pure e
    _ -> CuriousMoonNosyNuisance <$> liftRunMessage msg attrs
