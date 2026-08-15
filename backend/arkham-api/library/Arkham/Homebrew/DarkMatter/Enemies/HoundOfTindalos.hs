module Arkham.Homebrew.DarkMatter.Enemies.HoundOfTindalos (houndOfTindalos) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection

newtype HoundOfTindalos = HoundOfTindalos EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Spawn - Put Hound of Tindalos into play next to the act deck, at no specific
location."
-}
houndOfTindalos :: EnemyCard HoundOfTindalos
houndOfTindalos = enemyWith HoundOfTindalos Cards.houndOfTindalos (spawnAtL ?~ SpawnAt Nowhere)

{- | "Massive. Alert. Retaliate. / While you have an odd number of cards in your
hand, Hound of Tindalos is considered to be engaged with you."
-}
instance HasModifiersFor HoundOfTindalos where
  getModifiersFor (HoundOfTindalos a) = do
    modifySelf a [AddKeyword Keyword.Massive, AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]
    investigators <- select UneliminatedInvestigator
    odds <- filterM (fmap (odd . length) . field InvestigatorHand) investigators
    for_ odds \iid -> modifySelect a (InvestigatorWithId iid) [AsIfEngagedWith a.id]

instance RunMessage HoundOfTindalos where
  runMessage msg (HoundOfTindalos attrs) = HoundOfTindalos <$> runMessage msg attrs
