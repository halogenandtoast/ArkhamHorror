module Arkham.Asset.Assets.PrudenceDouglas (
  prudenceDouglas,
  PrudenceDouglas(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Strategy

newtype PrudenceDouglas = PrudenceDouglas AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prudenceDouglas :: AssetCard PrudenceDouglas
prudenceDouglas = allyWith PrudenceDouglas Cards.prudenceDouglas (2, 2) noSlots

instance HasAbilities PrudenceDouglas where
  getAbilities (PrudenceDouglas a) =
    [ controlledAbility a 1 (DuringTurn You)
        $ FastAbility (exhaust a <> assetUseCost a Portent 1)
    ]

instance RunMessage PrudenceDouglas where
  runMessage msg a@(PrudenceDouglas attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt
        iid
        (attrs.ability 1)
        EncounterDeckTarget
        [(fromTopOfDeck 4, PutBackInAnyOrder)]
        #any
        (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      let encounterCards = onlyEncounterCards cards
          discardable = filter (notElem Elite . toTraits) encounterCards
      focusCards encounterCards do
        chooseUpToNM iid 2 "Done discarding" do
          targets discardable $ \c -> push $ AddToEncounterDiscard c
      pure a
    _ -> PrudenceDouglas <$> liftRunMessage msg attrs
