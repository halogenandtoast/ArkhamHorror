{- | Chemical Firm (015) -- set the whole room off.

The blast hits investigators too, so the enemies take 'nonAttackEnemyDamage'
while the investigators get an 'assignDamage' they distribute themselves.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.ChemicalFirm_015 (chemicalFirm_015) where

import Arkham.Ability
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChemicalFirm_015 = ChemicalFirm_015 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chemicalFirm_015 :: LocationCard ChemicalFirm_015
chemicalFirm_015 = location ChemicalFirm_015 Cards.chemicalFirm_015 3 (PerPlayer 2)

instance HasAbilities ChemicalFirm_015 where
  getAbilities (ChemicalFirm_015 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "chemicalFirm.blast"
      $ groupLimit PerRound
      $ restricted a 1 Here actionAbility

instance RunMessage ChemicalFirm_015 where
  runMessage msg l@(ChemicalFirm_015 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyAt attrs.id) $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      selectEach (investigatorAt attrs) $ assignDamageTo (attrs.ability 1) 1
      pure l
    _ -> ChemicalFirm_015 <$> liftRunMessage msg attrs
