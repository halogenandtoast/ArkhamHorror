module Arkham.Homebrew.DarkMatter.Enemies.VoidByakhee (voidByakhee) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf, modifySelfMaybe)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype VoidByakhee = VoidByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | "Spawn - Any location without clues on it."
voidByakhee :: EnemyCard VoidByakhee
voidByakhee = enemy VoidByakhee Cards.voidByakhee & setSpawnAt LocationWithoutClues

{- | "Hunter. / Void Byakhee moves as if all locations without clues on them are
connected to each other." Modelled by making every clueless location connect to
every other clueless location for this enemy's movement.
-}
instance HasModifiersFor VoidByakhee where
  getModifiersFor (VoidByakhee a) = do
    modifySelf a [AddKeyword Keyword.Hunter]
    -- Hunter movement only: every clueless location counts as connected to the
    -- clueless location it currently occupies.
    clueless <- select LocationWithoutClues
    modifySelfMaybe a do
      here <- MaybeT $ getLocationOf a.id
      guard $ here `elem` clueless
      pure [HunterConnectedTo lid | lid <- clueless, lid /= here]

instance RunMessage VoidByakhee where
  runMessage msg (VoidByakhee attrs) = VoidByakhee <$> runMessage msg attrs
