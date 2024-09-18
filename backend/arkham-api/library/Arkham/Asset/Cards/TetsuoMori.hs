module Arkham.Asset.Cards.TetsuoMori (tetsuoMori, TetsuoMori (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (at, chooseOne)
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Script
import Arkham.Strategy

newtype TetsuoMori = TetsuoMori AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tetsuoMori :: AssetCard TetsuoMori
tetsuoMori = ally TetsuoMori Cards.tetsuoMori (2, 2)

instance HasModifiersFor TetsuoMori where
  getModifiersFor (InvestigatorTarget iid) (TetsuoMori a) | not (controlledBy a iid) = do
    maybeModified a do
      location <- MaybeT $ field InvestigatorLocation iid
      assetLocation <- MaybeT $ field AssetLocation a.id
      guard $ location == assetLocation
      pure [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]
  getModifiersFor _ _ = pure []

instance HasAbilities TetsuoMori where
  getAbilities (TetsuoMori a) =
    [ controlledAbility
        a
        1
        ( exists
            $ affectsOthers
            $ at_ YourLocation
            <> oneOf [can.have.cards.leaveDiscard <> DiscardWith #item, can.manipulate.deck]
        )
        (freeReaction $ Matcher.AssetDefeated #when ByAny $ be a)
    ]

-- Choose an investigator at your location. That investigator searches either their discard pile or the top 9 cards of their deck for an Item asset and adds it to their hand. Shuffle their deck if it is searched.

instance RunMessage TetsuoMori where
  runMessage = script do
    ability 1 do
      chooseOne (investigator `at` yourLocation) do
        onChoose \iid -> pickOne iid do
          hasInDiscard iid #item do
            labeled "Search Discard" $ search iid iid [fromDiscard] #item (DrawFound iid 1)
          labeled "Search Deck" $ search iid iid [fromTopOfDeck 9] #item (DrawFound iid 1)

-- runMessage msg a@(TetsuoMori attrs) = runQueueT $ case msg of
--   UseThisAbility iid (isSource attrs -> True) 1 -> do
--     iids <-
--       select
--         $ affectsOthers
--         $ colocatedWith iid
--         <> oneOf [can.have.cards.leaveDiscard <> DiscardWith #item, can.manipulate.deck]
--     chooseOrRunOneM iid $ targets iids $ handleTarget iid (attrs.ability 1)
--     pure a
--   HandleTargetChoice _ source@(isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
--     discardWithItem <- fieldP InvestigatorDiscard (any (`cardMatch` card_ #item)) iid'
--     chooseOrRunOneM iid' do
--       when discardWithItem do
--         labeled "Search Discard" $ search iid' source iid' [fromDiscard] #item (DrawFound iid' 1)
--       labeled "Search Deck" $ search iid' source iid' [fromTopOfDeck 9] #item (DrawFound iid' 1)
--     pure a
--   _ -> TetsuoMori <$> liftRunMessage msg attrs
