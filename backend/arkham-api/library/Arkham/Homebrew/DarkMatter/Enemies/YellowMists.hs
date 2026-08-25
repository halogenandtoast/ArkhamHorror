module Arkham.Homebrew.DarkMatter.Enemies.YellowMists (yellowMists) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype YellowMists = YellowMists EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

yellowMists :: EnemyCard YellowMists
yellowMists = enemy YellowMists Cards.yellowMists

{- | "Aloof. Hunter. / Yellow Mists' location gains the [[Carcosa]] trait." The
trait matters: the Carcosa-face abilities test for "the only [[Carcosa]] location
in play".
-}
instance HasModifiersFor YellowMists where
  getModifiersFor (YellowMists a) = do
    modifySelf a [AddKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]
    modifySelect a (locationWithEnemy a.id) [AddTrait Carcosa]

instance RunMessage YellowMists where
  runMessage msg (YellowMists attrs) = YellowMists <$> runMessage msg attrs
