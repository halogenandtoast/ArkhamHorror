module Arkham.Treachery.Cards.BoundInRed (boundInRed) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Types (Field (EnemyHealthActual))
import Arkham.Helpers.Calculation
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Coterie))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BoundInRed = BoundInRed TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boundInRed :: TreacheryCard BoundInRed
boundInRed = treachery BoundInRed Cards.boundInRed

instance HasModifiersFor BoundInRed where
  getModifiersFor (BoundInRed a) = for_ a.attached.enemy \enemy ->
    modified_ a enemy [EnemyFight 1, EnemyEvade 1, DamageDealt 1, HorrorDealt 1]

instance HasAbilities BoundInRed where
  getAbilities (BoundInRed a) =
    [restricted a 1 (exists (ScarletKeyWithBearer You <> StableScarletKey)) actionAbility]

instance RunMessage BoundInRed where
  runMessage msg t@(BoundInRed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      anyCoterie <- selectAny $ EnemyWithTrait Coterie
      if not anyCoterie
        then gainSurge attrs
        else do
          coterie <- selectMaxByM EnemyHealthActual (maybe (pure (-1)) calculate) $ EnemyWithTrait Coterie
          chooseTargetM iid coterie $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithBearer (InvestigatorWithId iid) <> StableScarletKey
      chooseTargetM iid skeys $ flipOverBy iid (attrs.ability 1)
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> BoundInRed <$> liftRunMessage msg attrs
