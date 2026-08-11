module Arkham.Homebrew.DarkMatter.Enemies.TheStranger (theStranger) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards

{- | The Man in the Pallid Mask's quarry. Its keywords (Aloof, Hunter) come from
its card definition.

TODO: The Stranger is the back face of the "Awakening?" act card
(@:dark-matter:194b@) and, like every back face in this set, its printed text is
absent from @dm-cards.json@. If it prints an ability beyond its keywords, it is
not modelled here.
-}
newtype TheStranger = TheStranger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStranger :: EnemyCard TheStranger
theStranger = enemy TheStranger Cards.theStranger

instance RunMessage TheStranger where
  runMessage msg (TheStranger attrs) = TheStranger <$> runMessage msg attrs
