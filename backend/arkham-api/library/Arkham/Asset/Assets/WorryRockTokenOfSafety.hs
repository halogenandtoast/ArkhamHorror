module Arkham.Asset.Assets.WorryRockTokenOfSafety (worryRockTokenOfSafety) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher

newtype WorryRockTokenOfSafety = WorryRockTokenOfSafety AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

worryRockTokenOfSafety :: AssetCard WorryRockTokenOfSafety
worryRockTokenOfSafety = assetWith WorryRockTokenOfSafety Cards.worryRockTokenOfSafety (sanityL ?~ 2)

instance HasAbilities WorryRockTokenOfSafety where
  getAbilities (WorryRockTokenOfSafety a) =
    [ playerLimit PerGame
        $ restricted a 1 InYourHand
        $ triggered_
        $ DiscardedFromHand #when You SourceIsScenarioCardEffect (basic $ CardWithId $ toCardId a)
    ]

instance RunMessage WorryRockTokenOfSafety where
  runMessage msg a@(WorryRockTokenOfSafety attrs) = runQueueT $ case msg of
    InHand _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      drawCards iid (attrs.ability 1) 3
      pure a
    _ -> WorryRockTokenOfSafety <$> liftRunMessage msg attrs
