module Arkham.Location.Cards.Pasture (pasture) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Window (getEnemy, getEnemyMovedVia)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype Pasture = Pasture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pasture :: LocationCard Pasture
pasture = symbolLabel $ locationWith Pasture Cards.pasture 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Pasture where
  getAbilities (Pasture a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> DuringTurn You <> exists (be a <> LocationWithDamage (atLeast 1)))
          $ FastAbility Free
      , restricted a 2 (not_ $ thisIs a LocationWithAdjacentBarrier)
          $ forced
          $ EnemyMovedTo #after (be a) (MovedViaOneOf [#hunter, #patrol]) (ReadyEnemy <> UnengagedEnemy)
      ]

instance RunMessage Pasture where
  runMessage msg l@(Pasture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ ConnectedTo ForMovement (be attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (getEnemyMovedVia &&& getEnemy -> (movedVia, enemy)) _ -> do
      case movedVia of
        MovedViaHunter -> push $ HunterMove enemy
        MovedViaPatrol -> push $ ForTarget (toTarget enemy) HuntersMove
        _ -> pure ()
      pure l
    _ -> Pasture <$> liftRunMessage msg attrs
