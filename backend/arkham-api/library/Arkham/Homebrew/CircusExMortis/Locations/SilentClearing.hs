module Arkham.Homebrew.CircusExMortis.Locations.SilentClearing (silentClearing) where

import Arkham.Ability
import Arkham.Aspect.Types (InsteadOf (..))
import Arkham.Calculation
import Arkham.Fight
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype SilentClearing = SilentClearing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silentClearing :: LocationCard SilentClearing
silentClearing = location SilentClearing Cards.silentClearing 2 (PerPlayer 2)

instance HasAbilities SilentClearing where
  getAbilities (SilentClearing a) =
    extendRevealed
      a
      [ -- "Spend 1-2 clues": a range of the payer's own clues, not a group cost, so
        -- 'AtLeastOne' over a 1-clue cost rather than 'GroupClueCostRange'.
        fightAbility a 1 (AtLeastOne (Fixed 2) (clueCost 1))
          $ Here
          <> exists (CanFightEnemy (toSource a))
      , -- Dodge's window, narrowed to non-Elite attackers. 'CancelableEnemyAttack'
        -- keeps the ability off the table for an attack that cannot be cancelled.
        restricted a 2 Here
          $ triggered
            ( EnemyAttacks
                #when
                (investigatorAt a)
                (CancelableEnemyAttack AnyEnemyAttack)
                NonEliteEnemy
            )
            (clueCost 1)
      ]

instance RunMessage SilentClearing where
  runMessage msg l@(SilentClearing attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> x) -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #intellect x, DamageDealt x]
      -- "uses [intellect] instead of [combat]" is the aspect, not a bare skill
      -- override: it substitutes only a test that would have used combat, and it
      -- honours CanIgnoreAspect (Empower Self).
      aspect iid source (#intellect `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 2) details
      pure l
    _ -> SilentClearing <$> liftRunMessage msg attrs
