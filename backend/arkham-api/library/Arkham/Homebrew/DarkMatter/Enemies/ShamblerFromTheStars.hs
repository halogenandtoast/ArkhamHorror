module Arkham.Homebrew.DarkMatter.Enemies.ShamblerFromTheStars (shamblerFromTheStars) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype ShamblerFromTheStars = ShamblerFromTheStars EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shamblerFromTheStars :: EnemyCard ShamblerFromTheStars
shamblerFromTheStars = enemy ShamblerFromTheStars Cards.shamblerFromTheStars

{- | "Aloof. Hunter. / Shambler from the Stars cannot be engaged except by the
ability below."
-}
instance HasModifiersFor ShamblerFromTheStars where
  getModifiersFor (ShamblerFromTheStars a) =
    modifySelf a [AddKeyword Keyword.Aloof, AddKeyword Keyword.Hunter, CannotBeEngaged]

{- | "Forced - After you trigger an [action] ability at its location: Shambler
from the Stars engages and attacks you. (Limit once per round.)"

Shambler has a scanning back, so the [action] ability that reveals it is normally
the very scan that puts it into play — and that activation's After frame opens
with Shambler already standing at your location. It was not in play when you
triggered that ability, so it must not answer it.

Meta is what it starts caring from. Ability 2 can only fire on a When frame that
opened after Shambler entered play (the entry-tick filter in
'Arkham.Helpers.Action' guarantees that, #4927), so it flips meta on the first
activation Shambler was actually present for — and only then does ability 1
exist to answer that same activation's After frame. Arming on 'Anyone' because
what matters is that Shambler was in play when the ability was triggered, not
whose ability it was.
-}
instance HasAbilities ShamblerFromTheStars where
  getAbilities (ShamblerFromTheStars a) =
    extend a
      $ if toResultDefault False a.meta
        then
          [ limitedAbility (PlayerLimit PerRound 1)
              $ restricted a 1 (youExist $ at_ (locationWithEnemy a.id))
              $ forced
              $ ActivateAbility #after You AbilityIsActionAbility
          ]
        else [mkAbility a 2 $ silent $ ActivateAbility #when Anyone AbilityIsActionAbility]

instance RunMessage ShamblerFromTheStars where
  runMessage msg e@(ShamblerFromTheStars attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      engageEnemy iid attrs
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 ->
      pure . ShamblerFromTheStars $ attrs & setMeta True
    _ -> ShamblerFromTheStars <$> liftRunMessage msg attrs
