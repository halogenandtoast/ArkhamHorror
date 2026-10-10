module Arkham.Homebrew.AgesUnwound.Locations.WinchmoreHouse (winchmoreHouse) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getAccessibleLocations)
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

{- | "While there is a ready enemy at Winchmore House, investigators here cannot
move except by the below ability." / "Enemies at this location get +1 fight and
+1 evade."
-}
instance HasModifiersFor WinchmoreHouse where
  getModifiersFor (WinchmoreHouse a) = do
    modifySelect a (enemyAt a.id) [EnemyFight 1, EnemyEvade 1]
    readyEnemyHere <- selectAny $ ReadyEnemy <> enemyAt a.id
    modifySelectWhen a readyEnemyHere (investigatorAt a.id) [CannotMove]

{- | "[action] Choose and discard 2 cards from your hand: Disengage from each
enemy engaged with you and move to a connecting location."

The move is made uncancelable, which is what carries the "except by the below
ability" exemption: the movement runner lets an uncancelable move through even
while the mover has 'CannotMove'. Only cancelability is cleared -- a full
'forcedMove' would also waive additional costs to enter, which this ability
never says it does.
-}
instance HasAbilities WinchmoreHouse where
  getAbilities (WinchmoreHouse a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (ConnectedFrom ForMovement $ be a))
      $ actionAbilityWithCost (HandDiscardCost 2 $ basic AnyCard)

instance RunMessage WinchmoreHouse where
  runMessage msg l@(WinchmoreHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      connected <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid connected \lid ->
        moveToEdit (attrs.ability 1) iid lid \m -> m {moveCancelable = False}
      pure l
    _ -> WinchmoreHouse <$> liftRunMessage msg attrs
