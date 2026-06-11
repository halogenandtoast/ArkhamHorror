module Arkham.Agenda.Cards.TheMastermind (theMastermind) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.DamageEffect (nonAttack)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Trait (Trait (Distortion))

newtype TheMastermind = TheMastermind AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMastermind :: AgendaCard TheMastermind
theMastermind = agenda (3, A) TheMastermind Cards.theMastermind (Static 7)

-- Each [[Distortion]] location gains the enciphered ability. Decoded with
-- Eixodolon's Note it reads: "[action] Spend 1 clue: Deal 3 damage to
-- Eixodolon. (Group limit once per game.)"
--
-- The printed objective (advance when each investigator is defeated) is
-- covered by the scenario's NoResolution handling, which leads to R1 just
-- like this agenda's back.
instance HasAbilities TheMastermind where
  getAbilities (TheMastermind a) =
    guard (onSide A a)
      *> [ groupLimit PerGame
             $ restricted
               (proxied (LocationWithTrait Distortion) a)
               1
               (Here <> exists (enemyIs Enemies.eixodolon))
             $ actionAbilityWithCost (ClueCost (Static 1))
         ]

instance RunMessage TheMastermind where
  runMessage msg a@(TheMastermind attrs) = runQueueT $ case msg of
    UseThisAbility _ p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      eixodolon <- selectJust $ enemyIs Enemies.eixodolon
      push $ DealDamage (EnemyTarget eixodolon) (nonAttack Nothing (AbilitySource p 1) 3)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- In this timeline, the ritual is complete.
      push R1
      pure a
    _ -> TheMastermind <$> liftRunMessage msg attrs
