module Arkham.Asset.Assets.IkiaqTheCouncilsChosen3 (
  ikiaqTheCouncilsChosen3,
  IkiaqTheCouncilsChosen3 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Card (drawThisCardFrom)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher

newtype IkiaqTheCouncilsChosen3 = IkiaqTheCouncilsChosen3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ikiaqTheCouncilsChosen3 :: AssetCard IkiaqTheCouncilsChosen3
ikiaqTheCouncilsChosen3 = allyWith IkiaqTheCouncilsChosen3 Cards.ikiaqTheCouncilsChosen3 (2, 2) $ setMeta @[CardId] []

instance HasModifiersFor IkiaqTheCouncilsChosen3 where
  getModifiersFor (IkiaqTheCouncilsChosen3 a) = do
    let n = 1 - count (`cardMatch` WeaknessCard) a.cardsUnderneath
    controllerGets a [SkillModifier #willpower n, SkillModifier #intellect n]

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
      cancelCardDraw attrs card
      obtainCard card
      let cardIds = toCardId card : toResult @[CardId] attrs.meta
      pure . IkiaqTheCouncilsChosen3 $ attrs & cardsUnderneathL %~ (card :) & setMeta cardIds
    RemovedFromPlay (isSource attrs -> True) -> do
      let cardIds = toResult @[CardId] attrs.meta
      let weaknesses = filter ((`elem` cardIds) . toCardId) attrs.cardsUnderneath
      for_ weaknesses \weakness -> for_ (toCardOwner weakness) \owner ->
        pushAll $ drawThisCardFrom owner weakness Nothing
      IkiaqTheCouncilsChosen3
        <$> lift (runMessage msg $ attrs & cardsUnderneathL %~ filter (`notElem` weaknesses))
    _ -> IkiaqTheCouncilsChosen3 <$> liftRunMessage msg attrs
