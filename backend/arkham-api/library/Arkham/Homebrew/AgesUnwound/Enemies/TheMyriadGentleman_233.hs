module Arkham.Homebrew.AgesUnwound.Enemies.TheMyriadGentleman_233 (theMyriadGentleman_233) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Traits (pattern Exterior)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTowards)
import Arkham.Trait (Trait (Ritual))

newtype TheMyriadGentleman_233 = TheMyriadGentleman_233 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /The High Priest/. Alert and Retaliate are on the card def.
theMyriadGentleman_233 :: EnemyCard TheMyriadGentleman_233
theMyriadGentleman_233 = enemy TheMyriadGentleman_233 Cards.theMyriadGentleman_233

{- | "The Myriad Gentleman cannot move to a non-[[Ritual]] location."

Held as @CannotBeEnteredBy@ on the locations rather than @CannotEnter@ on the
enemy: both are honoured by @LocationCanBeEnteredBy@ (which is what @getPaths@
filters moves through), but only @CannotBeEnteredBy@ is also honoured by
@EnemyCanEnter@.
-}
instance HasModifiersFor TheMyriadGentleman_233 where
  getModifiersFor (TheMyriadGentleman_233 a) =
    modifySelect a (not_ $ LocationWithTrait Ritual) [CannotBeEnteredBy (be a)]

{- | "Forced - After The Myriad Gentleman attacks you: Move once towards the
nearest [[Exterior]] location."

/You/ move, not the Gentleman. The imperative names no subject, so it addresses
the same "you" the window does -- the investigator he just attacked -- and he
drives them out of the house a step at a time. Read the other way the ability
could never do anything at all: the ban above keeps him off every
non-@[[Ritual]]@ location, and the way out is Exterior.

"The nearest" is therefore measured from the investigator, and a tie is theirs
to break.
-}
instance HasAbilities TheMyriadGentleman_233 where
  getAbilities (TheMyriadGentleman_233 a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage TheMyriadGentleman_233 where
  runMessage msg e@(TheMyriadGentleman_233 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nearest <- select $ NearestLocationTo iid (LocationWithTrait Exterior)
      chooseOrRunOneM iid $ targets nearest (moveTowards (attrs.ability 1) iid)
      pure e
    _ -> TheMyriadGentleman_233 <$> liftRunMessage msg attrs
