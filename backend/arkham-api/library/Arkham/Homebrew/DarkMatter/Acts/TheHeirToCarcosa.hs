module Arkham.Homebrew.DarkMatter.Acts.TheHeirToCarcosa (theHeirToCarcosa) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipSurroundingLocations)
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Matcher
import Arkham.Trait (Trait (Cave))

newtype TheHeirToCarcosa = TheHeirToCarcosa ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Objective - Find the Royal Princess of Carcosa. (You will be instructed
when to advance)" — so this act never advances itself.
-}
theHeirToCarcosa :: ActCard TheHeirToCarcosa
theHeirToCarcosa = act (2, A) TheHeirToCarcosa Cards.theHeirToCarcosa Nothing

{- | "[free] If you are at a [[Cave]] or [[Carcosa]] location, spend
1[per_investigator] clues, as a group: Flip your location and all connecting
locations to their other side."
-}
instance HasAbilities TheHeirToCarcosa where
  getAbilities (TheHeirToCarcosa a) =
    [ restricted a 1 (OnLocation $ oneOf [LocationWithTrait Cave, LocationWithTrait Carcosa])
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    ]

instance RunMessage TheHeirToCarcosa where
  runMessage msg a@(TheHeirToCarcosa attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipSurroundingLocations iid (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheHeirToCarcosa <$> liftRunMessage msg attrs
