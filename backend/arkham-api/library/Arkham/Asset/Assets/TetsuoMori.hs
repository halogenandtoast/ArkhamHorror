module Arkham.Asset.Assets.TetsuoMori (tetsuoMori, TetsuoMori (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Item))

newtype TetsuoMori = TetsuoMori AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tetsuoMori :: AssetCard TetsuoMori
tetsuoMori = ally TetsuoMori Cards.tetsuoMori (2, 2)

instance HasModifiersFor TetsuoMori where
  getModifiersFor (TetsuoMori a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      modifySelect
        a
        (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
        [CanAssignDamageToAsset (toId a), CanAssignHorrorToAsset (toId a)]

instance HasAbilities TetsuoMori where
  getAbilities (TetsuoMori a) =
    [ controlledAbility
        a
        1
        ( exists
            $ affectsOthers
            $ InvestigatorAt YourLocation
            <> oneOf
              [ can.have.cards.leaveDiscard <> DiscardWith #item
              , can.manipulate.deck
              ]
        )
        $ freeReaction (Matcher.AssetDefeated #when ByAny $ AssetWithId $ toId a)
    ]

instance RunMessage TetsuoMori where
  runMessage msg a@(TetsuoMori attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <-
        select
          $ affectsOthers
          $ colocatedWith iid
          <> AnyInvestigator
            [ can.have.cards.leaveDiscard <> DiscardWith #item
            , can.manipulate.deck
            ]
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ targetLabels iids
        $ only
        . handleTargetChoice iid (attrs.ability 1)
      pure a
    HandleTargetChoice _ source@(isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      discardWithItem <- fieldP InvestigatorDiscard (any (`cardMatch` CardWithTrait Item)) iid'
      player <- getPlayer iid'
      push
        $ chooseOrRunOne player
        $ [ Label "Search Discard" [search iid' source iid' [fromDiscard] #item (DrawFound iid' 1)]
          | discardWithItem
          ]
        <> [Label "Search Deck" [search iid' source iid' [fromTopOfDeck 9] #item (DrawFound iid' 1)]]
      pure a
    _ -> TetsuoMori <$> runMessage msg attrs
