module Arkham.Asset.Assets.Fence1 (fence1, Fence1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier

newtype Fence1 = Fence1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fence1 :: AssetCard Fence1
fence1 = asset Fence1 Cards.fence1

instance HasModifiersFor Fence1 where
  getModifiersFor (InvestigatorTarget iid) (Fence1 a) | controlledBy a iid && not (assetExhausted a) = do
    toModifiers a [CanBecomeFast #illicit, CanReduceCostOf (#illicit <> FastCard) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities Fence1 where
  getAbilities (Fence1 a) =
    [ controlled a 1 (DuringTurn You)
        $ ReactionAbility (Matcher.PlayCard #when You (basic #illicit))
        $ exhaust a
    ]

instance RunMessage Fence1 where
  runMessage msg a@(Fence1 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      let source = attrs.ability 1
      if isFastCard card
        then costModifier source iid (ReduceCostOf (CardWithId $ toCardId card) 1)
        else cardResolutionModifier card source card (BecomesFast FastPlayerWindow)
      pure a
    _ -> Fence1 <$> liftRunMessage msg attrs
