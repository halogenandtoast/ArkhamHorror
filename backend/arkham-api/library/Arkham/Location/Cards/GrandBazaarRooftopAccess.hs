module Arkham.Location.Cards.GrandBazaarRooftopAccess (grandBazaarRooftopAccess) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Helpers.Window (getEnemy, getEnemyMovedVia)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype GrandBazaarRooftopAccess = GrandBazaarRooftopAccess LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarRooftopAccess :: LocationCard GrandBazaarRooftopAccess
grandBazaarRooftopAccess =
  locationWith
    GrandBazaarRooftopAccess
    Cards.grandBazaarRooftopAccess
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities GrandBazaarRooftopAccess where
  getAbilities (GrandBazaarRooftopAccess a) =
    extendRevealed
      a
      [ skillTestAbility
          $ mkAbility a 1
          $ freeReaction
          $ Moves #after You AnySource (connectedTo (be a)) (be a)
      , mkAbility a 2 $ forced $ EnemyMovedTo #after (be a) (MovedViaOneOf [#hunter, #patrol]) AnyEnemy
      ]

instance RunMessage GrandBazaarRooftopAccess where
  runMessage msg l@(GrandBazaarRooftopAccess attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (getEnemyMovedVia &&& getEnemy -> (movedVia, enemy)) _ -> do
      case movedVia of
        MovedViaHunter -> push $ HunterMove enemy
        MovedViaPatrol -> push $ ForTarget (toTarget enemy) HuntersMove
        _ -> pure ()
      pure l
    _ -> GrandBazaarRooftopAccess <$> liftRunMessage msg attrs
