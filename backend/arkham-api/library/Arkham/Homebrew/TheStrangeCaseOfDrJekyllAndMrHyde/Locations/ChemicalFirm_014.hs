{- | Chemical Firm (014) -- pay a point of damage to finish an enemy off.

The enemy clause rides on the window's 'EnemyMatcher'. @after DealtDamage@ opens
once the damage has landed, so a lethal hit has already discarded the enemy and
'enemyAt' no longer holds -- which is what we want: there is nothing left to deal
the additional damage to, so the ability is not offered and the 1 damage is not
paid for nothing.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.ChemicalFirm_014 (chemicalFirm_014) where

import Arkham.Ability
import Arkham.Helpers.Window (damagedEnemy)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChemicalFirm_014 = ChemicalFirm_014 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chemicalFirm_014 :: LocationCard ChemicalFirm_014
chemicalFirm_014 = location ChemicalFirm_014 Cards.chemicalFirm_014 3 (PerPlayer 2)

instance HasAbilities ChemicalFirm_014 where
  getAbilities (ChemicalFirm_014 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "chemicalFirm.reaction"
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ triggered
        (EnemyDealtDamage #after AnyDamageEffect (enemyAt a.id) (SourceUsedBy You))
        (DamageCost (a.ability 1) YouTarget 1)

instance RunMessage ChemicalFirm_014 where
  runMessage msg l@(ChemicalFirm_014 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (damagedEnemy -> eid) _ -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure l
    _ -> ChemicalFirm_014 <$> liftRunMessage msg attrs
