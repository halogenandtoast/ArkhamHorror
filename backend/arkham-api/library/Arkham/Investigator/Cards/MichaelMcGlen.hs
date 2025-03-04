module Arkham.Investigator.Cards.MichaelMcGlen (michaelMcGlen) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype MichaelMcGlen = MichaelMcGlen InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

michaelMcGlen :: InvestigatorCard MichaelMcGlen
michaelMcGlen =
  investigator MichaelMcGlen Cards.michaelMcGlen
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 1, combat = 5, agility = 3}

instance HasAbilities MichaelMcGlen where
  getAbilities (MichaelMcGlen a) =
    [ restricted a 1 (Self <> youExist can.gain.resources)
        $ freeReaction (SpentUses #after You AnySource Ammo (#firearm <> assetMatcher) (atLeast 1))
    ]
   where
    assetMatcher = case lookupMetaKeyWithDefault "perFirearmAssetPerRound" [] a of
      [] -> AnyAsset
      xs -> not_ (mapOneOf AssetWithId xs)

instance HasChaosTokenValue MichaelMcGlen where
  getChaosTokenValue iid ElderSign (MichaelMcGlen attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getFirearm :: [Window] -> AssetId
getFirearm [] = error "Missing firearm"
getFirearm ((windowType -> Window.SpentUses _ _ aid _ _) : _) = aid
getFirearm (_ : rest) = getFirearm rest

instance RunMessage MichaelMcGlen where
  runMessage msg i@(MichaelMcGlen attrs) = runQueueT $ case msg of
    BeginRound -> do
      inner <- liftRunMessage msg attrs
      pure $ MichaelMcGlen $ setMetaKey "perFirearmAssetPerRound" ([] :: [AssetId]) inner
    UseCardAbility iid (isSource attrs -> True) 1 (getFirearm -> aid) _ -> do
      let firearms :: [AssetId] = lookupMetaKeyWithDefault "perFirearmAssetPerRound" [] attrs
      gainResources iid (attrs.ability 1) 1
      pure $ MichaelMcGlen $ setMetaKey "perFirearmAssetPerRound" (aid : firearms) attrs
    ElderSignEffect iid | iid == attrs.id -> do
      firearms <- select $ assetControlledBy iid <> #firearm
      unless (null firearms) do
        chooseOneM iid do
          labeled "Do not add any ammo" nothing
          targets firearms \firearm -> addUses ElderSign firearm Ammo 1
      pure i
    _ -> MichaelMcGlen <$> liftRunMessage msg attrs
