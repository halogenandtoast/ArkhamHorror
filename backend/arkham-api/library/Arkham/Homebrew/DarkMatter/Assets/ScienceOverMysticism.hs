module Arkham.Homebrew.DarkMatter.Assets.ScienceOverMysticism (scienceOverMysticism) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types qualified as Asset
import Arkham.Card
import Arkham.Event.Types qualified as Event
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ritual, Spell))

newtype ScienceOverMysticism = ScienceOverMysticism AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scienceOverMysticism :: AssetCard ScienceOverMysticism
scienceOverMysticism = asset ScienceOverMysticism Cards.scienceOverMysticism

spellOrRitual :: (OneOf a, WithTrait a) => a
spellOrRitual = hasAnyTrait [Spell, Ritual]

{- | "[action] Discard a Spell or Ritual under your control or from your hand:
Draw 1 card and gain X resources, where X is the resource cost of the discarded
card."

The discard spans three zones — an asset in play, an event in play, a card in
hand — and no 'Cost' reaches all three, so it is resolved in the handler. The
criterion is that same disjunction, so the action is never offered with nothing
to discard.
-}
instance HasAbilities ScienceOverMysticism where
  getAbilities (ScienceOverMysticism a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists $ DiscardableAsset <> AssetControlledBy You <> spellOrRitual
            , exists $ EventControlledBy You <> spellOrRitual
            , exists $ basic (DiscardableCard <> spellOrRitual) <> InHandOf NotForPlay You
            ]
        )
        actionAbility
    ]

instance RunMessage ScienceOverMysticism where
  runMessage msg a@(ScienceOverMysticism attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      assets <-
        selectWithField Asset.AssetCard $ DiscardableAsset <> assetControlledBy iid <> spellOrRitual
      events <- selectWithField Event.EventCard $ eventControlledBy iid <> spellOrRitual
      hand <- select $ basic (DiscardableCard <> spellOrRitual) <> inHandOf NotForPlay iid
      let
        cashIn card doDiscard = do
          doDiscard
          drawCards iid source 1
          gainResources iid source (toCardDef card).printedCost
      chooseOneM iid do
        for_ assets \(aid, card) -> targeting aid $ cashIn card $ toDiscardBy iid source aid
        for_ events \(eid, card) -> targeting eid $ cashIn card $ toDiscardBy iid source eid
        for_ hand \card -> targeting card $ cashIn card $ discardCard iid source card
      pure a
    _ -> ScienceOverMysticism <$> liftRunMessage msg attrs
