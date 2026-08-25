module Arkham.Homebrew.DarkMatter.Acts.TheHeirToCarcosa (theHeirToCarcosa) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Helpers.Agenda (getDoomOnAgenda)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
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
    {- Act 2b:

    "Advance to agenda 2a and act 3a. Do not remove doom from play. Move all
    doom from agenda 1 to agenda 2a.
    Spawn the set aside Cave Dweller enemy at the Abandoned Lander."

    'do_' on 'AdvanceToAgenda' is what keeps the doom in play: the plain message
    is handled by the agenda runner, which prefixes a 'RemoveAllDoomFromPlay'. -}
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      carriedOver <- getDoomOnAgenda
      do_ $ AdvanceToAgenda 1 Agendas.shallDryAndDie Agenda.A (toSource attrs)
      placeDoomOnAgenda carriedOver
      createSetAsideEnemy_ Enemies.caveDweller Locations.abandonedLander
      advanceActDeck attrs
      pure a
    _ -> TheHeirToCarcosa <$> liftRunMessage msg attrs
