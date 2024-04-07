module Arkham.Asset.Cards.IkiaqTheCouncilsChosen3 (
  ikiaqTheCouncilsChosen3,
  IkiaqTheCouncilsChosen3 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Card (drawThisCard, getCardEntityTarget)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Message

newtype IkiaqTheCouncilsChosen3 = IkiaqTheCouncilsChosen3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ikiaqTheCouncilsChosen3 :: AssetCard IkiaqTheCouncilsChosen3
ikiaqTheCouncilsChosen3 = allyWith IkiaqTheCouncilsChosen3 Cards.ikiaqTheCouncilsChosen3 (2, 2) $ setMeta @[CardId] []

instance HasModifiersFor IkiaqTheCouncilsChosen3 where
  getModifiersFor (InvestigatorTarget iid) (IkiaqTheCouncilsChosen3 a) | a `controlledBy` iid = do
    let n = 1 - count (`cardMatch` WeaknessCard) a.cardsUnderneath
    pure $ toModifiers a [SkillModifier #willpower n, SkillModifier #intellect n]
  getModifiersFor _ _ = pure []

instance HasAbilities IkiaqTheCouncilsChosen3 where
  getAbilities (IkiaqTheCouncilsChosen3 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          ( DrawCard
              #when
              (affectsOthers $ InvestigatorAt YourLocation)
              (CanCancelRevelationEffect $ basic BasicWeaknessCard)
              AnyDeck
          )
          (exhaust x)
    ]

instance RunMessage IkiaqTheCouncilsChosen3 where
  runMessage msg (IkiaqTheCouncilsChosen3 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      target <- getCardEntityTarget card
      let cardIds = toCardId card : toResult @[CardId] attrs.meta
      pushAll
        [ CancelEachNext (toSource attrs) [RevelationMessage, DrawEnemyMessage]
        , CancelSurge (toSource attrs)
        , QuietlyRemoveFromGame target
        ]
      pure . IkiaqTheCouncilsChosen3 $ attrs & cardsUnderneathL %~ (card :) & setMeta cardIds
    RemovedFromPlay (isSource attrs -> True) -> do
      let cardIds = toResult @[CardId] attrs.meta
      let weaknesses = filter ((`elem` cardIds) . toCardId) attrs.cardsUnderneath
      for_ weaknesses \weakness -> for_ (toCardOwner weakness) \owner ->
        pushAll $ drawThisCard owner weakness
      IkiaqTheCouncilsChosen3
        <$> lift (runMessage msg $ attrs & cardsUnderneathL %~ filter (`notElem` weaknesses))
    _ -> IkiaqTheCouncilsChosen3 <$> lift (runMessage msg attrs)
