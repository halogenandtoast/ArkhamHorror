module Arkham.Homebrew.DarkMatter.Treacheries.RadioactiveDecay (radioactiveDecay) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawRandomFacedownCard)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype RadioactiveDecay = RadioactiveDecay TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

radioactiveDecay :: TreacheryCard RadioactiveDecay
radioactiveDecay = treachery RadioactiveDecay Cards.radioactiveDecay

{- | "Revelation - Put this card into play, in your threat area.
Forced - At the end of your turn: Draw a face-down card in your threat area. If
you cannot, take 1 damage and 1 horror instead.
[action][action]: Discard Radioactive Decay."
-}
instance HasAbilities RadioactiveDecay where
  getAbilities (RadioactiveDecay a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    , restricted a 2 (InThreatAreaOf You) $ doubleActionAbilityWithCost mempty
    ]

instance RunMessage RadioactiveDecay where
  runMessage msg t@(RadioactiveDecay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drew <- drawRandomFacedownCard iid
      unless drew $ assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> RadioactiveDecay <$> liftRunMessage msg attrs
