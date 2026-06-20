module Arkham.Enemy.Cards.SheldonGang (sheldonGang) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Trait (Trait (Criminal))

newtype SheldonGang = SheldonGang EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sheldonGang :: EnemyCard SheldonGang
sheldonGang = enemy SheldonGang Cards.sheldonGang

instance HasModifiersFor SheldonGang where
  getModifiersFor (SheldonGang a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities SheldonGang where
  getAbilities (SheldonGang a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ PlayCard #after (InvestigatorAt $ locationWithEnemy a.id) (basic AnyCard)

instance RunMessage SheldonGang where
  runMessage msg e@(SheldonGang attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readyThis attrs
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 3 attrs
      pure e
    DiscardedTopOfEncounterDeck _iid cards _ (isTarget attrs -> True) -> do
      withLocationOf attrs \lid -> do
        let criminals = filterCards (CardWithTrait Criminal) (map toCard cards)
        for_ criminals \card -> createEnemyAt_ card lid
      pure e
    _ -> SheldonGang <$> liftRunMessage msg attrs
