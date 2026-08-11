module Arkham.Homebrew.DarkMatter.Acts.TheUnspeakableTruth (theUnspeakableTruth) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipSurroundingLocations)
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))

newtype TheUnspeakableTruth = TheUnspeakableTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnspeakableTruth :: ActCard TheUnspeakableTruth
theUnspeakableTruth = act (3, A) TheUnspeakableTruth Cards.theUnspeakableTruth Nothing

{- | The same flip ability act 2 prints, plus
"Objective - If each undefeated investigator has resigned, advance."
-}
instance HasAbilities TheUnspeakableTruth where
  getAbilities (TheUnspeakableTruth a) =
    [ restricted a 1 (OnLocation $ oneOf [LocationWithTrait Cave, LocationWithTrait Carcosa])
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    , restricted a 2 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheUnspeakableTruth where
  runMessage msg a@(TheUnspeakableTruth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipSurroundingLocations iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheUnspeakableTruth <$> liftRunMessage msg attrs
