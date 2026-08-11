module Arkham.Homebrew.DarkMatter.Locations.BrainStorage (brainStorage) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.GameValue
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (brainsAttachedTo)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain, pattern Interface)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype BrainStorage = BrainStorage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brainStorage :: LocationCard BrainStorage
brainStorage = location BrainStorage Cards.brainStorage 4 (Static 0)

{- | "Forced - After you reveal Brain Storage: Attach each set aside [[Brain]]
story asset to it." / "[action] Choose a [[Brain]] story asset in play: Attach
the chosen asset to any [[Interface]] location."
-}
instance HasAbilities BrainStorage where
  getAbilities (BrainStorage a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> exists (AssetWithTrait Brain)) actionAbility
      ]

{- | The other [[Interface]] locations print "Limit 2 [[Brain]] story assets
attached to this location."; Brain Storage itself prints no limit, since it
starts holding every brain.
-}
canHoldBrain :: HasGame m => LocationAttrs -> LocationId -> m Bool
canHoldBrain attrs lid
  | lid == attrs.id = pure True
  | otherwise = (< 2) . length <$> brainsAttachedTo lid

instance RunMessage BrainStorage where
  runMessage msg l@(BrainStorage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      brains <- getSetAsideCardsMatching (CardWithTrait Brain)
      for_ brains \c -> createAssetAt_ c (AttachedToLocation attrs.id)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      brains <- select $ AssetWithTrait Brain
      chooseTargetM iid brains \aid -> do
        locations <- filterM (canHoldBrain attrs) =<< select (LocationWithTrait Interface)
        chooseTargetM iid locations \lid -> push $ PlaceAsset aid (AttachedToLocation lid)
      pure l
    _ -> BrainStorage <$> liftRunMessage msg attrs
