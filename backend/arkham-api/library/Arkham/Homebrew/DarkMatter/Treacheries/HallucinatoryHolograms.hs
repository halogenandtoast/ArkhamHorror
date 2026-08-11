module Arkham.Homebrew.DarkMatter.Treacheries.HallucinatoryHolograms (hallucinatoryHolograms) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype HallucinatoryHolograms = HallucinatoryHolograms TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallucinatoryHolograms :: TreacheryCard HallucinatoryHolograms
hallucinatoryHolograms = treachery HallucinatoryHolograms Cards.hallucinatoryHolograms

{- | "Hidden. Peril. Revelation - Secretly add this card to your hand.
Forced - At the end of your turn, if there is at least 1 [[AI]] encounter card
in your threat area: Take 2 horror and discard Hallucinatory Holograms.
[action][action]: Discard Hallucinatory Holograms."
-}
instance HasAbilities HallucinatoryHolograms where
  getAbilities (HallucinatoryHolograms a) =
    [ restricted a 1 (youExist $ HasMatchingTreachery $ TreacheryWithTrait AI)
        $ forced
        $ TurnEnds #when You
    , restricted a 2 OnSameLocation $ doubleActionAbilityWithCost mempty
    ]

instance RunMessage HallucinatoryHolograms where
  runMessage msg t@(HallucinatoryHolograms attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 2
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> HallucinatoryHolograms <$> liftRunMessage msg attrs
