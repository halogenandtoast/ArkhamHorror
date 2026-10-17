module Arkham.Homebrew.AgesUnwound.Locations.WinchmoreHouse (winchmoreHouse) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectWhen)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveToEdit)
import Arkham.Movement (Movement (moveCancelable))

newtype WinchmoreHouse = WinchmoreHouse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winchmoreHouse :: LocationCard WinchmoreHouse
winchmoreHouse = symbolLabel $ location WinchmoreHouse Cards.winchmoreHouse 2 (PerPlayer 1)

instance HasModifiersFor WinchmoreHouse where
  getModifiersFor (WinchmoreHouse a) = do
    modifySelect a (enemyAt a.id) [EnemyFight 1, EnemyEvade 1]
    readyEnemyHere <- selectAny $ ReadyEnemy <> enemyAt a.id
    modifySelectWhen a readyEnemyHere (investigatorAt a.id) [CannotMove]

instance HasAbilities WinchmoreHouse where
  getAbilities (WinchmoreHouse a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (ConnectedFrom ForMovement $ be a))
      $ actionAbilityWithCost (HandDiscardCost 2 $ basic AnyCard)

instance RunMessage WinchmoreHouse where
  runMessage msg l@(WinchmoreHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      connected <- select $ ConnectedFrom ForMovement (be attrs)
      chooseTargetM iid connected \lid ->
        moveToEdit (attrs.ability 1) iid lid \m -> m {moveCancelable = False}
      pure l
    _ -> WinchmoreHouse <$> liftRunMessage msg attrs
