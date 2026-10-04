module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.UnderwaterCavern (underwaterCavern) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator)
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Cave, DeepOne))

newtype UnderwaterCavern = UnderwaterCavern LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underwaterCavern :: LocationCard UnderwaterCavern
underwaterCavern =
  locationWith UnderwaterCavern Cards.underwaterCavern 2 (PerPlayer 1) connectsToAdjacent

{- | "While moving, Deep One investigators and Deep One enemies treat Underwater Cavern
as if it were connected to every flooded Cave location."

The connection belongs to the mover, not to the location -- a non-Deep-One standing here
does not get it -- so it is granted as 'MovesAsIfConnectedTo' on each Deep One
investigator and enemy. Both directions are granted, since "treat X as connected to Y"
is symmetric: a Deep One here may leave for a flooded Cave, and a Deep One at a flooded
Cave may come here.
-}
instance HasModifiersFor UnderwaterCavern where
  getModifiersFor (UnderwaterCavern a) = whenRevealed a do
    let floodedCaves = FloodedLocation <> LocationWithTrait Cave
    modifySelect a (InvestigatorAt (be a) <> deepOneInvestigator) [MovesAsIfConnectedTo floodedCaves]
    modifySelect a (EnemyAt (be a) <> EnemyWithTrait DeepOne) [MovesAsIfConnectedTo floodedCaves]
    modifySelect a (InvestigatorAt floodedCaves <> deepOneInvestigator) [MovesAsIfConnectedTo (be a)]
    modifySelect a (EnemyAt floodedCaves <> EnemyWithTrait DeepOne) [MovesAsIfConnectedTo (be a)]

instance HasAbilities UnderwaterCavern where
  getAbilities (UnderwaterCavern a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists (not_ (be a) <> moveDestinations))
          $ ActionAbility #move Nothing (ActionCost 1)
      , mkAbility a 2 $ forced $ RevealLocation #after Anyone (be a)
      ]

-- | "Move from Underwater Cavern to another Location named Underwater Cavern or Underground River."
moveDestinations :: LocationMatcher
moveDestinations =
  oneOf [LocationWithTitle "Underwater Cavern", LocationWithTitle "Underground River"]

instance RunMessage UnderwaterCavern where
  runMessage msg l@(UnderwaterCavern attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ not_ (be attrs) <> moveDestinations
      chooseTargetM iid ls $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure $ UnderwaterCavern $ attrs & floodLevelL ?~ FullyFlooded
    _ -> UnderwaterCavern <$> liftRunMessage msg attrs
