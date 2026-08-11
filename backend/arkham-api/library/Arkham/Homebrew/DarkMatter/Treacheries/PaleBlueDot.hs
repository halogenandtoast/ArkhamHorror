module Arkham.Homebrew.DarkMatter.Treacheries.PaleBlueDot (paleBlueDot) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype PaleBlueDot = PaleBlueDot TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paleBlueDot :: TreacheryCard PaleBlueDot
paleBlueDot = treachery PaleBlueDot Cards.paleBlueDot

{- | "Revelation - Attach Pale Blue Dot to your current location.
Forced - After you leave attached location, you must either (choose one): Take 1
horror, or add 1 doom to the current agenda and discard this card. This effect
may cause the agenda to advance."
-}
instance HasAbilities PaleBlueDot where
  getAbilities (PaleBlueDot a) =
    [mkAbility a 1 $ forced $ Leaves #after You (LocationWithTreachery $ TreacheryWithId a.id)]

instance RunMessage PaleBlueDot where
  runMessage msg t@(PaleBlueDot attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ campaignI18n do
        labeled' "paleBlueDot.takeHorror" $ assignHorror iid (attrs.ability 1) 1
        labeled' "paleBlueDot.placeDoom" do
          placeDoomOnAgendaAndCheckAdvanceBy (attrs.ability 1) 1
          toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PaleBlueDot <$> liftRunMessage msg attrs
