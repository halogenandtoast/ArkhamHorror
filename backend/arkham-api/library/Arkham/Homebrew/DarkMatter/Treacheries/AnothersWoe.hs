module Arkham.Homebrew.DarkMatter.Treacheries.AnothersWoe (anothersWoe) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted

newtype AnothersWoe = AnothersWoe TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anothersWoe :: TreacheryCard AnothersWoe
anothersWoe = treachery AnothersWoe Cards.anothersWoe

{- | "Revelation - Put this card into play next to the agenda deck.
Forced - After an [[Ally]] asset leaves play: Place 1 doom on Another's Woe.
[action] Test [willpower] (3) or [agility] (3): If you succeed, discard
Another's Woe."
-}
instance HasAbilities AnothersWoe where
  getAbilities (AnothersWoe a) =
    [ mkAbility a 1 $ forced $ AssetLeavesPlay #after #ally
    , skillTestAbility $ restricted a 2 NoRestriction actionAbility
    ]

instance RunMessage AnothersWoe where
  runMessage msg t@(AnothersWoe attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ for_ [#willpower, #agility] \skill ->
        skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> AnothersWoe <$> liftRunMessage msg attrs
