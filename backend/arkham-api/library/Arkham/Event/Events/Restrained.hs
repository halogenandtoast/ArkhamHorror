module Arkham.Event.Events.Restrained (restrained) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher
import Arkham.Trait (Trait (Humanoid))

newtype Restrained = Restrained EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrained :: EventCard Restrained
restrained = event Restrained Cards.restrained

instance HasModifiersFor Restrained where
  getModifiersFor (Restrained a) = for_ a.attachedTo.enemy \enemy -> modified_ a enemy [CannotReady]

instance HasAbilities Restrained where
  getAbilities (Restrained a) =
    case a.attachedTo.enemy of
      Just enemy ->
        [ restricted a 1 (exists $ EnemyWithId enemy <> not_ (withTrait Humanoid)) $ forced $ RoundEnds #when
        ]
      _ -> []

instance RunMessage Restrained where
  runMessage msg e@(Restrained attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let details = getAttackDetails attrs.windows
      automaticallyEvadeEnemy iid details.enemy
      place attrs $ AttachedToEnemy details.enemy
      pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscardBy attrs.controller (attrs.ability 1) attrs
      pure e
    _ -> Restrained <$> liftRunMessage msg attrs
