module Arkham.Homebrew.AgesUnwound.Enemies.SavageYeti (savageYeti) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Matcher

newtype SavageYeti = SavageYeti EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Retaliate is on the def; /Enemy of My Enemy/ spawns it at the Himalayas.
savageYeti :: EnemyCard SavageYeti
savageYeti = enemy SavageYeti Cards.savageYeti

-- | "If Savage Yeti is damaged, it gets +2 fight and +2 evade."
instance HasModifiersFor SavageYeti where
  getModifiersFor (SavageYeti a) =
    modifySelfWhen a (a.damage > 0) [EnemyFight 2, EnemyEvade 2]

{- | "__Forced__ - If Savage Yeti is defeated: Flip this card over and resolve
its text."

@#when@ rather than @#after@: the defeat's disposal has not run yet, so
/Gratitude/'s "Remove this card from the game" can still reach the enemy instead
of leaving the card in the encounter discard. 'Window.IfEnemyDefeated' is only
ever announced @#after@ (see 'Arkham.Behavior.Defeat.closeDefeat'), so the
"is defeated" wording is carried by @EnemyDefeated \#when@.
-}
instance HasAbilities SavageYeti where
  getAbilities (SavageYeti a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage SavageYeti where
  runMessage msg e@(SavageYeti attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.gratitude
      pure e
    _ -> SavageYeti <$> liftRunMessage msg attrs
