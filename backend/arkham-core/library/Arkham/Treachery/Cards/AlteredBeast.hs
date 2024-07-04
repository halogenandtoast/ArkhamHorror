module Arkham.Treachery.Cards.AlteredBeast (AlteredBeast (..), alteredBeast) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype AlteredBeast = AlteredBeast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alteredBeast :: TreacheryCard AlteredBeast
alteredBeast = treachery AlteredBeast Cards.alteredBeast

instance HasAbilities AlteredBeast where
  getAbilities (AlteredBeast x) = case x.attached of
    Just (EnemyTarget eid) ->
      [ mkAbility x 1
          $ forced
          $ oneOf
            [EnemyEnters #when YourLocation $ EnemyWithId eid, Enters #when You (locationWithEnemy eid)]
      ]
    _ -> []

instance RunMessage AlteredBeast where
  runMessage msg t@(AlteredBeast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectTargets (EnemyWithTrait Abomination) >>= \case
        [] -> gainSurge attrs
        xs ->
          chooseOrRunOne iid
            $ [ targetLabel x [Msg.attachTreachery attrs x, HealAllDamage x (toSource attrs)]
              | x <- xs
              ]
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      pure t
    _ -> AlteredBeast <$> liftRunMessage msg attrs
