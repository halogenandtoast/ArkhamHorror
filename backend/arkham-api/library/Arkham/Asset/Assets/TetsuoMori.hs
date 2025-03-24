module Arkham.Asset.Assets.TetsuoMori (tetsuoMori) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Strategy
import Arkham.Trait (Trait (Item))

newtype TetsuoMori = TetsuoMori AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tetsuoMori :: AssetCard TetsuoMori
tetsuoMori = ally TetsuoMori Cards.tetsuoMori (2, 2)

instance HasModifiersFor TetsuoMori where
  getModifiersFor (TetsuoMori a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset (toId a), CanAssignHorrorToAsset (toId a)]

instance HasAbilities TetsuoMori where
  getAbilities (TetsuoMori a) =
    [ controlled a 1 (exists $ affectsOthers $ at_ YourLocation <> validExists)
        $ freeReaction
        $ AssetDefeated #when ByAny (be a)
    ]

validExists :: InvestigatorMatcher
validExists = oneOf [can.have.cards.leaveDiscard <> DiscardWith #item, can.manipulate.deck]

instance RunMessage TetsuoMori where
  runMessage msg a@(TetsuoMori attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> validExists
      chooseOrRunOneM iid $ targets iids $ handleTarget iid (attrs.ability 1)
      pure a
    HandleTargetChoice _ source@(isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      discardWithItem <- fieldP InvestigatorDiscard (any (`cardMatch` CardWithTrait Item)) iid'
      chooseOrRunOneM iid' do
        when discardWithItem do
          labeled "Search Discard" $ search iid' source iid' [fromDiscard] #item (DrawFound iid' 1)
        labeled "Search Deck" $ search iid' source iid' [fromTopOfDeck 9] #item (DrawFound iid' 1)
      pure a
    _ -> TetsuoMori <$> liftRunMessage msg attrs
