module Arkham.Homebrew.CircusExMortis.Treacheries.BrokenCouplings (brokenCouplings) where

import Arkham.Ability
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Location (getConnectedLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers, modified_)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement (place)
import Arkham.Name (toTitle)
import Arkham.Placement
import Arkham.Projection (fieldMap)
import Arkham.Trait (Trait (Train))
import Arkham.Treachery.Import.Lifted

newtype Meta = Meta {otherLocation :: LocationId}
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

newtype BrokenCouplings = BrokenCouplings TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenCouplings :: TreacheryCard BrokenCouplings
brokenCouplings = treachery BrokenCouplings Cards.brokenCouplings

otherLocationOf :: TreacheryAttrs -> LocationId -> LocationId
otherLocationOf attrs locA = otherLocation (toResultDefault (Meta locA) attrs.meta)

instance HasModifiersFor BrokenCouplings where
  getModifiersFor (BrokenCouplings attrs) = case attrs.placement of
    AttachedToLocation locA -> do
      let locB = otherLocationOf attrs locA
      when (locB /= locA) do
        modified_ attrs (LocationTarget locA) [Barricades [locB]]
        modified_ attrs (LocationTarget locB) [Barricades [locA]]
    _ -> pure ()

instance HasAbilities BrokenCouplings where
  getAbilities (BrokenCouplings attrs) = case attrs.placement of
    AttachedToLocation locA -> do
      let locB = otherLocationOf attrs locA
      [ restricted attrs 1 (youExist $ at_ (mapOneOf LocationWithId [locA, locB]))
          $ actionAbilityWithCost (ResourceCost 2)
        ]
    _ -> []

unblockedTrainPairs :: HasGame m => m [(LocationId, LocationId)]
unblockedTrainPairs = do
  trainLocations <- select $ LocationWithTrait Train
  blockedPairs <- concatForM trainLocations \loc -> do
    mods <- getModifiers loc
    pure [(loc, other) | Barricades others <- mods, other <- others]
  let
    blocked (a, b) = (a, b) `elem` blockedPairs || (b, a) `elem` blockedPairs
    dedupe (a, b) = if unLocationId a <= unLocationId b then (a, b) else (b, a)
  candidates <- concatForM trainLocations \locA -> do
    conns <- filterM (<=~> LocationWithTrait Train) =<< getConnectedLocations locA
    pure [dedupe (locA, locB) | locB <- conns, locA /= locB, not (blocked (locA, locB))]
  pure $ nub candidates

instance RunMessage BrokenCouplings where
  runMessage msg t@(BrokenCouplings attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      candidates <- unblockedTrainPairs
      chooseOneM iid do
        for_ (zip [0 :: Int ..] candidates) \(idx, (locA, locB)) -> do
          nameA <- fieldMap LocationName toTitle locA
          nameB <- fieldMap LocationName toTitle locB
          ( campaignI18n
              $ withVars ["locationA" .= nameA, "locationB" .= nameB]
              $ labeled "brokenCouplings.placeBetween"
            )
            do
              push $ DoStep idx (Revelation iid (toSource attrs))
      pure t
    DoStep idx (Revelation _ (isSource attrs -> True)) -> do
      mPair <- (!!? idx) <$> unblockedTrainPairs
      case mPair of
        Nothing -> pure t
        Just (locA, locB) -> do
          place attrs (AttachedToLocation locA)
          pure . BrokenCouplings $ setMeta (Meta locB) attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid attrs attrs
      pure t
    _ -> BrokenCouplings <$> liftRunMessage msg attrs
