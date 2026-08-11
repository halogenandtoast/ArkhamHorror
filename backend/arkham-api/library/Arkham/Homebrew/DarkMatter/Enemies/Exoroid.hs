module Arkham.Homebrew.DarkMatter.Enemies.Exoroid (exoroid) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (shuffleIntoScanningDeck)
import Arkham.Homebrew.DarkMatter.Traits (pattern AsteroidBelt)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype Exoroid = Exoroid EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exoroid :: EnemyCard Exoroid
exoroid = enemy Exoroid Cards.exoroid & setSpawnAt (LocationWithTrait AsteroidBelt)

-- | "Hunter. Retaliate."
instance HasModifiersFor Exoroid where
  getModifiersFor (Exoroid a) = modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate]

{- | "Forced - After you defeat Exoroid: Shuffle it back into the scanning deck."
(The campaign rule for scanning-back cards: they never leave that deck.)
-}
instance HasAbilities Exoroid where
  getAbilities (Exoroid a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)

instance RunMessage Exoroid where
  runMessage msg e@(Exoroid attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      shuffleIntoScanningDeck [toCard attrs]
      pure e
    _ -> Exoroid <$> liftRunMessage msg attrs
