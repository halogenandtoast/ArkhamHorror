module Arkham.Treachery.Cards.AlteredBeast (alteredBeast) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AlteredBeast = AlteredBeast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alteredBeast :: TreacheryCard AlteredBeast
alteredBeast = treachery AlteredBeast Cards.alteredBeast

instance HasAbilities AlteredBeast where
  getAbilities (AlteredBeast x) = case x.attached.enemy of
    Just eid ->
      [ mkAbility x 1
          $ forced
          $ oneOf
            [EnemyEnters #when YourLocation $ EnemyWithId eid, Enters #when You (locationWithEnemy eid)]
      ]
    _ -> []

instance RunMessage AlteredBeast where
  runMessage msg t@(AlteredBeast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      select (EnemyWithTrait Abomination) >>= \case
        [] -> gainSurge attrs
        xs -> chooseTargetM iid xs \x -> attachTreachery attrs x >> healAllDamage attrs x
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      pure t
    _ -> AlteredBeast <$> liftRunMessage msg attrs
