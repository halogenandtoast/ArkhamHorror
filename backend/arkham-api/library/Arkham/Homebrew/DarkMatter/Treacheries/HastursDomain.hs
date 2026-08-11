module Arkham.Homebrew.DarkMatter.Treacheries.HastursDomain (hastursDomain) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype HastursDomain = HastursDomain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hastursDomain :: TreacheryCard HastursDomain
hastursDomain = treachery HastursDomain Cards.hastursDomain

{- | "Revelation - Put Hastur's Domain into play next to the agenda deck. /
Forced - After you flip any number of locations: Take 1 horror. / [action]: Test
[willpower] (3) or [agility] (3). If you succeed, discard Hastur's Domain."
-}
instance HasAbilities HastursDomain where
  getAbilities (HastursDomain a) =
    [ mkAbility a 1 $ forced $ FlipLocation #after You Anywhere
    , mkAbility a 2 actionAbility
    ]

instance RunMessage HastursDomain where
  runMessage msg t@(HastursDomain attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ for_ [#willpower, #agility] \skill ->
        skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> HastursDomain <$> liftRunMessage msg attrs
