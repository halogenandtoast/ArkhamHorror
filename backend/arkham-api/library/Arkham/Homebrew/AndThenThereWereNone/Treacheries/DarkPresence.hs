-- | "__Revelation__ - Attach Dark Presence to your location."
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.DarkPresence (darkPresence) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier), modified_, modifySelect)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype DarkPresence = DarkPresence TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkPresence :: TreacheryCard DarkPresence
darkPresence = treachery DarkPresence Cards.darkPresence

{- | "Attached location gets +2 shroud value. Each connecting location gets +1
shroud value."
-}
instance HasModifiersFor DarkPresence where
  getModifiersFor (DarkPresence a) = for_ a.attached.location \lid -> do
    modified_ a lid [ShroudModifier 2]
    modifySelect a (connectedFrom (LocationWithId lid)) [ShroudModifier 1]

{- | "{action}{action}: Discard Dark Presence. If there are 1 or fewer cards in
your hand, reduce the cost of this ability by 1 action."

One action always, plus a second one unless the hand is down to 1 card --
'CostWhen', so the reduction applies on its own rather than being offered.
-}
instance HasAbilities DarkPresence where
  getAbilities (DarkPresence a) =
    [ scenarioI18n
        $ withI18nTooltip "darkPresence.dispel"
        $ restricted a 1 OnSameLocation
        $ actionAbilityWithCost
        $ CostWhen (youExist $ HandWith $ LengthIs $ atLeast 2) (ActionCost 1)
    ]

instance RunMessage DarkPresence where
  runMessage msg t@(DarkPresence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DarkPresence <$> liftRunMessage msg attrs
