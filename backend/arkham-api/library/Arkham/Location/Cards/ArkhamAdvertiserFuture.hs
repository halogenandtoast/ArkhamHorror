module Arkham.Location.Cards.ArkhamAdvertiserFuture (arkhamAdvertiserFuture) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag (getOnlyChaosTokensInBag)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token

newtype ArkhamAdvertiserFuture = ArkhamAdvertiserFuture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAdvertiserFuture :: LocationCard ArkhamAdvertiserFuture
arkhamAdvertiserFuture = location ArkhamAdvertiserFuture Cards.arkhamAdvertiserFuture 2 (PerPlayer 1)

instance HasAbilities ArkhamAdvertiserFuture where
  getAbilities (ArkhamAdvertiserFuture a) =
    extendRevealed
      a
      [ limitedAbility (GroupLimit PerGame 2) $ restricted a 1 Here actionAbility
      , restricted a 2 Here $ FastAbility (SpendTokenCost Token.Newspaper (TargetIs $ toTarget a))
      ]

instance RunMessage ArkhamAdvertiserFuture where
  runMessage msg l@(ArkhamAdvertiserFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      newspaperLocations <-
        select
          $ mapOneOf
            locationIs
            [Cards.arkhamGazette, Cards.arkhamAdvertiserPresent, Cards.arkhamAdvertiserFuture]
      chooseTargetM iid newspaperLocations \dest ->
        placeTokens (attrs.ability 1) dest Token.Newspaper 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- perPlayer 1
      placeClues (attrs.ability 2) attrs n
      inBag <- map (.face) <$> getOnlyChaosTokensInBag
      let faces = filter (`elem` inBag) [Skull, Cultist, Tablet, ElderThing]
      chooseOrRunOneM iid do
        for_ faces \face -> do
          chaosTokenLabeled face $ push $ RemoveChaosToken face
      pure l
    _ -> ArkhamAdvertiserFuture <$> liftRunMessage msg attrs
