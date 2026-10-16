{- | "Revelation - Attach Aggressive Mutation to a non-/Elite/ enemy without a
copy of Aggressive Mutation attached. If Aggressive Mutation cannot be attached
to a valid enemy, it gains surge."
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.AggressiveMutation (
  aggressiveMutation,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Abomination, Elite))
import Arkham.Treachery.Import.Lifted hiding (EnemyEvaded)

newtype AggressiveMutation = AggressiveMutation TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aggressiveMutation :: TreacheryCard AggressiveMutation
aggressiveMutation = treachery AggressiveMutation Cards.aggressiveMutation

{- | "Attached enemy gains the /Abomination/ trait, gets +2 fight, +1 damage, and
+1 horror." An enemy's traits fold trait modifiers, so the grant is 'AddTrait'.
-}
instance HasModifiersFor AggressiveMutation where
  getModifiersFor (AggressiveMutation a) = for_ a.attached.enemy \eid ->
    modified_ a eid [AddTrait Abomination, EnemyFight 2, DamageDealt 1, HorrorDealt 1]

instance HasAbilities AggressiveMutation where
  getAbilities (AggressiveMutation a) =
    [ scenarioI18n
        $ withI18nTooltip "aggressiveMutation.discard"
        $ mkAbility a 1
        $ forced
        $ EnemyEvaded #after You (EnemyWithAttachedTreachery (be a))
    ]

instance RunMessage AggressiveMutation where
  runMessage msg t@(AggressiveMutation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <-
        select
          $ not_ (EnemyWithTrait Elite)
          <> not_ (EnemyWithAttachedTreachery $ treacheryIs Cards.aggressiveMutation)
      if null enemies
        then gainSurge attrs
        else chooseTargetM iid enemies (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> AggressiveMutation <$> liftRunMessage msg attrs
