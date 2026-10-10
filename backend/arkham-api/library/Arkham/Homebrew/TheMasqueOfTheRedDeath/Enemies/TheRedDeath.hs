module Arkham.Homebrew.TheMasqueOfTheRedDeath.Enemies.TheRedDeath (theRedDeath) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyHealthDamage))
import Arkham.Helpers.Doom (getDoomOnTarget)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Traits (pattern Victim)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Humanoid))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype TheRedDeath = TheRedDeath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedDeath :: EnemyCard TheRedDeath
theRedDeath = enemy TheRedDeath Cards.theRedDeath

instance HasModifiersFor TheRedDeath where
  -- Printed health is a dash, and "The Red Death cannot be engaged."
  getModifiersFor (TheRedDeath a) = modifySelf a [CannotBeDamaged, CannotBeEngaged]

instance HasAbilities TheRedDeath where
  getAbilities (TheRedDeath a) =
    extend
      a
      [ -- "Forced - When the enemy phase begins: The Red Death attacks each
        -- investigator, Humanoid enemy, and Victim story asset at its location."
        restricted a 1 anyPrey $ forced $ PhaseBegins #when #enemy
      , -- "Forced - When a card is defeated by The Red Death: Move each doom on
        -- that card to the current agenda."
        mkAbility a 2
          $ forced
          $ oneOf
            [ Matcher.EnemyDefeated #when Anyone byRedDeath AnyEnemy
            , Matcher.AssetDefeated #when byRedDeath AnyAsset
            , Matcher.InvestigatorDefeated #when byRedDeath Anyone
            ]
      ]
   where
    here = locationWithEnemy a.id
    byRedDeath = BySource $ SourceIsEnemy (be a)
    anyPrey =
      oneOf
        [ exists $ investigatorAt here
        , exists $ EnemyAt here <> EnemyWithTrait Humanoid
        , exists $ AssetAt here <> AssetWithTrait Victim
        ]

-- | The card the defeat window is about, whichever kind of card it was.
defeatedCard :: [Window] -> Target
defeatedCard = \case
  (windowType -> Window.EnemyDefeated _ _ eid) : _ -> EnemyTarget eid
  (windowType -> Window.AssetDefeated aid _) : _ -> AssetTarget aid
  (windowType -> Window.InvestigatorDefeated _ iid) : _ -> InvestigatorTarget iid
  _ : rest -> defeatedCard rest
  [] -> error "TheRedDeath: no defeat window"

instance RunMessage TheRedDeath where
  runMessage msg e@(TheRedDeath attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        selectEach (investigatorAt lid) $ initiateEnemyAttack attrs (attrs.ability 1)
        -- an enemy is not a legal attack target, so it takes the Red Death's
        -- damage directly; horror cannot be assigned to an enemy
        dmg <- field EnemyHealthDamage attrs.id
        selectEach (enemyAt lid <> EnemyWithTrait Humanoid)
          $ nonAttackEnemyDamage Nothing (attrs.ability 1) dmg
        selectEach (assetAt lid <> AssetWithTrait Victim)
          $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 (defeatedCard -> target) _ -> do
      n <- getDoomOnTarget target
      when (n > 0) do
        removeDoom (attrs.ability 2) target n
        placeDoomOnAgendaBy (attrs.ability 2) n
      pure e
    _ -> TheRedDeath <$> liftRunMessage msg attrs
