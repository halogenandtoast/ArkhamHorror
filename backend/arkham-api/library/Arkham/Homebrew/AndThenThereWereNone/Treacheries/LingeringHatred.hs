-- | "__Revelation__ - Put Lingering Hatred into play next to the agenda deck."
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.LingeringHatred (lingeringHatred) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window.Enemy (getEnemy)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Geist))
import Arkham.Treachery.Import.Lifted hiding (EnemyEvaded)

newtype LingeringHatred = LingeringHatred TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lingeringHatred :: TreacheryCard LingeringHatred
lingeringHatred = treachery LingeringHatred Cards.lingeringHatred

-- | "Each _Geist_ enemy gets +1 damage and +1 horror."
instance HasModifiersFor LingeringHatred where
  getModifiersFor (LingeringHatred a) =
    modifySelect a (EnemyWithTrait Geist) [DamageDealt 1, HorrorDealt 1]

{- | Ability 1 carries "Each _Geist_ enemy gets +1 fight during the round that it
entered play." Nothing records when an enemy entered play, so the bonus is hung
on the enemy as it arrives -- a 'silent' forced, which is a constant effect at a
fixed timing with no prompt, not an ability the card prints.

Ability 2 is the printed "__Forced__ - After a _Geist_ enemy is evaded: Discard
Lingering Hatred."
-}
instance HasAbilities LingeringHatred where
  getAbilities (LingeringHatred a) =
    [ mkAbility a 1 $ silent $ EnemyEntersPlay #after (EnemyWithTrait Geist)
    , scenarioI18n
        $ withI18nTooltip "lingeringHatred.spent"
        $ mkAbility a 2
        $ forced
        $ EnemyEvaded #after Anyone (EnemyWithTrait Geist)
    ]

instance RunMessage LingeringHatred where
  runMessage msg t@(LingeringHatred attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemy -> eid) _ -> do
      roundModifier (attrs.ability 1) eid (EnemyFight 1)
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> LingeringHatred <$> liftRunMessage msg attrs
