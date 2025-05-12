module Arkham.Event.Events.Doppelganger1 (doppelganger1) where

import Arkham.Ability
import Arkham.Evade.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Placement

newtype Doppelganger1 = Doppelganger1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doppelganger1 :: EventCard Doppelganger1
doppelganger1 = event Doppelganger1 Cards.doppelganger1

instance HasAbilities Doppelganger1 where
  getAbilities (Doppelganger1 a) = case a.attachedTo.location of
    Just lid ->
      [ restricted a 1 OwnsThis
          $ triggered
            ( oneOf
                [ EnemyEnters #after (be lid) (EnemyCanBeEvadedBy (a.ability 1))
                , EnemyLeaves #after (be lid) (EnemyCanBeEvadedBy (a.ability 1))
                ]
            )
            (ReturnEventToHandCost a.id)
      , restricted a 1 (OwnsThis <> exists (CanMoveToLocation You (a.ability 1) (be lid)))
          $ triggered
            (oneOf [EnemyEnters #after (be lid) AnyEnemy, EnemyLeaves #after (be lid) AnyEnemy])
            (ReturnEventToHandCost a.id)
      ]
    _ -> []

instance RunMessage Doppelganger1 where
  runMessage msg e@(Doppelganger1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    PlaceEvent (is attrs -> True) (AttachedToLocation lid) -> do
      attrs' <- liftRunMessage msg attrs
      pure $ Doppelganger1 $ setMeta lid $ attrs' & waitingL .~ True
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemies -> enemies) _ -> do
      case getEventMeta attrs of
        Nothing -> error "Tried to trigger Doppelganger (1), but not location was recorded"
        Just lid -> do
          chooseOrRunOneM iid do
            targets enemies \eid -> do
              canEvade <- matches eid $ EnemyCanBeEvadedBy (attrs.ability 1)
              canMove <- matches iid $ InvestigatorCanMoveTo (attrs.ability 1) (LocationWithId lid)
              chooseOneM iid do
                when canMove do
                  labeled "Move to location of doppelganger" $ moveTo (attrs.ability 1) iid lid
                when canEvade do
                  labeled "Evade enemy" do
                    sid <- getRandom
                    chooseEvadeEnemyEdit sid iid (attrs.ability 1) \c ->
                      c {chooseEvadeEnemyMatcher = EnemyWithId eid, chooseEvadeOverride = True}
      pure e
    _ -> Doppelganger1 <$> liftRunMessage msg attrs
