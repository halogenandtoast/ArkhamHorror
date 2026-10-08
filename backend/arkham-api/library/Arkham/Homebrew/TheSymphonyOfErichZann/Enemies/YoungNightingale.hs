module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.YoungNightingale (youngNightingale) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveTo)

newtype YoungNightingale = YoungNightingale EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngNightingale :: EnemyCard YoungNightingale
youngNightingale = enemy YoungNightingale Cards.youngNightingale

instance HasAbilities YoungNightingale where
  getAbilities (YoungNightingale a) =
    extend
      a
      [ -- Drawing a Music treachery pulls it to you for an attack.
        mkAbility a 1 $ forced $ DrawCard #after Anyone (basic $ CardWithTrait Music) AnyDeck
      , -- Its attack can be bought off by drawing from the encounter deck.
        restricted a 2 (youExist $ at_ (locationWithEnemy a.id))
          $ freeReaction (EnemyAttacks #when You (CancelableEnemyAttack AnyEnemyAttack) (be a))
      ]

instance RunMessage YoungNightingale where
  runMessage msg e@(YoungNightingale attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      unless attrs.exhausted do
        lid <- selectJust $ locationWithInvestigator iid
        enemyMoveTo (attrs.ability 1) attrs lid
        initiateEnemyAttack attrs (attrs.ability 1) iid
        exhaustThis attrs
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 (getAttackDetails -> details) _ -> do
      -- "...draw the top card of the encounter deck: Cancel that attack."
      drawEncounterCard iid (attrs.ability 2)
      cancelAttack (attrs.ability 2) details
      pure e
    _ -> YoungNightingale <$> liftRunMessage msg attrs
