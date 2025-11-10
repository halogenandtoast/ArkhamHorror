module Arkham.Location.Cards.CafeLunaBastionOfRemembrance (cafeLunaBastionOfRemembrance) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Outsider))

newtype CafeLunaBastionOfRemembrance = CafeLunaBastionOfRemembrance LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cafeLunaBastionOfRemembrance :: LocationCard CafeLunaBastionOfRemembrance
cafeLunaBastionOfRemembrance =
  symbolLabel
    $ location CafeLunaBastionOfRemembrance Cards.cafeLunaBastionOfRemembrance 5 (PerPlayer 1)

instance HasModifiersFor CafeLunaBastionOfRemembrance where
  getModifiersFor (CafeLunaBastionOfRemembrance a) = do
    n <- selectCount $ InPlayEnemy $ EnemyWithTrait Outsider
    when (n > 0) $ modifySelf a [ShroudModifier (-n)]

instance HasAbilities CafeLunaBastionOfRemembrance where
  getAbilities (CafeLunaBastionOfRemembrance a) =
    extendRevealed1 a $ restricted a 1 (thisExists a LocationWithoutClues) $ forced AnyWindow

instance RunMessage CafeLunaBastionOfRemembrance where
  runMessage msg l@(CafeLunaBastionOfRemembrance attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs =<< perPlayer 1
      pure l
    _ -> CafeLunaBastionOfRemembrance <$> liftRunMessage msg attrs
