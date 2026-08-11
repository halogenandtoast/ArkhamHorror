module Arkham.Homebrew.DarkMatter.Enemies.DomaagTeel (domaagTeel) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DomaagTeel = DomaagTeel EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

domaagTeel :: EnemyCard DomaagTeel
domaagTeel = enemy DomaagTeel Cards.domaagTeel

-- | "Massive."
instance HasModifiersFor DomaagTeel where
  getModifiersFor (DomaagTeel a) = modifySelf a [AddKeyword Keyword.Massive]

{- | "Forced - After you defeat Domaag T'eel: Gain 3[per_investigator] clues from
the token bank."
-}
instance HasAbilities DomaagTeel where
  getAbilities (DomaagTeel a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)

instance RunMessage DomaagTeel where
  runMessage msg e@(DomaagTeel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 3
      gainClues iid (attrs.ability 1) n
      pure e
    _ -> DomaagTeel <$> liftRunMessage msg attrs
