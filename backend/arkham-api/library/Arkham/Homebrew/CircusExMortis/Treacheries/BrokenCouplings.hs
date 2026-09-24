module Arkham.Homebrew.CircusExMortis.Treacheries.BrokenCouplings (brokenCouplings) where

import Arkham.Ability
import Arkham.Helpers.Location (getConnectedLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Train))
import Arkham.Treachery.Import.Lifted

newtype BrokenCouplings = BrokenCouplings TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenCouplings :: TreacheryCard BrokenCouplings
brokenCouplings = treachery BrokenCouplings Cards.brokenCouplings

instance HasModifiersFor BrokenCouplings where
  getModifiersFor (BrokenCouplings attrs) = case attrs.placement of
    BetweenLocations locA locB -> do
      modified_ attrs (LocationTarget locA) [Barricades [locB]]
      modified_ attrs (LocationTarget locB) [Barricades [locA]]
    _ -> pure ()

instance HasAbilities BrokenCouplings where
  getAbilities (BrokenCouplings attrs) = case attrs.placement of
    BetweenLocations locA locB ->
      [ restricted attrs 1 (youExist $ at_ (mapOneOf LocationWithId [locA, locB]))
          $ actionAbilityWithCost (ResourceCost 2)
      ]
    _ -> []

instance RunMessage BrokenCouplings where
  runMessage msg t@(BrokenCouplings attrs) = runQueueT $ case msg of
    {- "Revelation - Place Broken Couplings between two Train locations without a
    copy between them." -}
    Revelation iid (isSource attrs -> True) -> do
      -- as with Entangled, "without a copy between them" is derived from where
      -- the copies already in play sit, there is no matcher for it
      existing <- select $ treacheryIs Cards.brokenCouplings <> not_ (TreacheryWithId attrs.id)
      taken <- traverse (field TreacheryPlacement) existing
      trainLocations <- select $ LocationWithTrait Train
      -- Picking a connection is two location clicks, not one prompt per pair:
      -- pick a car, then the car on the other side of the coupling.
      options <- forToSnd trainLocations \locA -> do
        conns <- filterM (<=~> LocationWithTrait Train) =<< getConnectedLocations locA
        pure [locB | locB <- conns, locB /= locA, betweenLocations locA locB `notElem` taken]
      chooseOrRunOneM iid do
        targets [locA | (locA, locBs) <- options, notNull locBs] \locA -> do
          let locBs = concat [bs | (a, bs) <- options, a == locA]
          chooseOrRunOneM iid $ targets locBs (placeTreachery attrs . betweenLocations locA)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid attrs attrs
      pure t
    _ -> BrokenCouplings <$> liftRunMessage msg attrs
