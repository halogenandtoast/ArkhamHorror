module Arkham.Asset.Assets.PrudenceDouglas (prudenceDouglas) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Token (Token (Portent))
import Arkham.Trait (Trait (Elite), toTraits)

newtype PrudenceDouglas = PrudenceDouglas AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
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
        [(FromTopOfDeck 4, PutBackInAnyOrder)]
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
