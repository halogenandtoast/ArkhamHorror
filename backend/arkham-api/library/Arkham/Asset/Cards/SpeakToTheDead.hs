module Arkham.Asset.Cards.SpeakToTheDead (speakToTheDead, SpeakToTheDead (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card.Id
import Arkham.ChaosBag.RevealStrategy
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy
import Arkham.Token

newtype Meta = Meta {chosenCard :: Maybe CardId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SpeakToTheDead = SpeakToTheDead (AssetAttrs `With` Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

speakToTheDead :: AssetCard SpeakToTheDead
speakToTheDead = asset (SpeakToTheDead . (`with` Meta Nothing)) Cards.speakToTheDead

instance HasAbilities SpeakToTheDead where
  getAbilities (SpeakToTheDead (With a _)) =
    [ controlledAbility
        a
        1
        (exists (InDiscardOf You <> oneOf [#spell, #ritual] <> #event) <> can.have.cards.leaveDiscard You)
        $ parleyAction
        $ UseCostUpTo (be a) Offering 1 (a.use Offering)
    ]

instance RunMessage SpeakToTheDead where
  runMessage msg a@(SpeakToTheDead (With attrs meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> x) -> do
      cards <- select $ InDiscardOf You <> oneOf [#spell, #ritual] <> #event
      focusCards cards \unfocus ->
        chooseOne iid [targetLabel card [unfocus, handleTargetChoice iid attrs card] | card <- cards]

      pushAll
        [ RequestChaosTokens (toSource attrs) (Just iid) (Reveal x) SetAside
        , ResetChaosTokens (toSource attrs)
        ]
      pure a
    HandleTargetChoice _iid (isSource attrs -> True) (CardIdTarget cid) -> do
      pure $ SpeakToTheDead $ attrs `with` Meta (Just cid)
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      when (any (`elem` map (.face) tokens) [#skull, #curse]) do
        for_ (chosenCard meta) \cid -> do
          card <- getCard cid
          obtainCard card
          addToHand iid [card]
      pure a
    _ -> SpeakToTheDead . (`with` meta) <$> liftRunMessage msg attrs
