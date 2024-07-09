module Arkham.Event.Cards.FriendsInLowPlaces (friendsInLowPlaces, FriendsInLowPlaces (..)) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayable)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Cost
import Arkham.Helpers.Customization
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Window (defaultWindows)
import Arkham.Zone

newtype Metadata = Metadata {chosenCards :: [CardId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FriendsInLowPlaces = FriendsInLowPlaces (With EventAttrs Metadata)
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

friendsInLowPlaces :: EventCard FriendsInLowPlaces
friendsInLowPlaces = event (FriendsInLowPlaces . (`with` Metadata [])) Cards.friendsInLowPlaces

instance HasModifiersFor FriendsInLowPlaces where
  getModifiersFor target (FriendsInLowPlaces (With a _)) | isTarget a target = do
    modified a $ guard (a `hasCustomization` Prompt) *> [BecomesFast FastPlayerWindow]
  getModifiersFor _ _ = pure []

instance RunMessage FriendsInLowPlaces where
  runMessage msg e@(FriendsInLowPlaces (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      targets <- select $ affectsOthers $ colocatedWith iid
      if notNull targets && attrs `hasCustomization` Helpful
        then chooseOrRunOne iid $ targetLabels targets $ only . handleTargetChoice iid attrs
        else push $ HandleTargetChoice iid (toSource attrs) (toTarget iid)
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      let traits = [t | ChosenTrait t <- concatMap snd (toList attrs.customizations)]
      lookAt
        iid'
        attrs
        iid'
        [fromTopOfDeck $ if attrs `hasCustomization` Experienced then 9 else 6]
        (basic $ oneOf $ CardWithTrait <$> traits)
        (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) x cards -> do
      when (notNull cards) do
        n <- getSpendableResources iid
        let traits = [t | ChosenTrait t <- concatMap snd (toList attrs.customizations)]
        let hasBothTraits = filterCards (foldMap CardWithTrait traits) cards
        if (attrs `hasCustomization` Versatile && notNull hasBothTraits && n > 0)
          then do
            chooseOne iid $ Label "Do not add a card to your hand for free (Versatile)" [DoStep 0 msg]
              : [ targetLabel
                  card
                  $ [Msg.phaseModifier attrs card (AddSkillIcons [#wild]) | attrs `hasCustomization` Bolstering]
                  <> [ AddToHand iid [card]
                     , handleTargetChoice iid attrs card
                     , DoStep 0 (SearchFound iid (toTarget attrs) x cards')
                     ]
                | (card, cards') <- eachWithRest hasBothTraits
                ]
          else push $ DoStep 0 msg
      when (attrs `hasCustomization` Clever) do
        chooseOne
          iid
          [ Label "Shuffle Cards Back In" []
          , Label
              "Place on the top of your deck, in any order"
              [UpdateSearchReturnStrategy iid FromDeck PutBackInAnyOrder]
          ]
      pushWhen (attrs `hasCustomization` Swift) $ Do msg
      pure e
    DoStep _ (SearchFound iid (isTarget attrs -> True) x cards) | notNull cards -> do
      n <- getSpendableResources iid
      when (n > 0) do
        chooseOne iid $ Label "Do not spend 1 resource to add a card to your hand" []
          : [ targetLabel
              card
              $ [SpendResources iid 1]
              <> [Msg.phaseModifier attrs card (AddSkillIcons [#wild]) | attrs `hasCustomization` Bolstering]
              <> [ AddToHand iid [card]
                 , handleTargetChoice iid attrs card
                 , DoStep 0 (SearchFound iid (toTarget attrs) x cards')
                 ]
            | (card, cards') <- eachWithRest cards
            ]
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (CardIdTarget cid) -> do
      pure . FriendsInLowPlaces $ attrs `with` Metadata (cid : chosenCards meta)
    Do (SearchFound iid (isTarget attrs -> True) _ _) -> do
      when (notNull $ chosenCards meta) do
        cards <- traverse getCard (chosenCards meta)
        playable <- filterM (getIsPlayable iid attrs (UnpaidCost NoAction) (defaultWindows iid)) cards
        when (notNull playable) do
          chooseOne iid $ Label "Do no play cards (Swift)" []
            : [targetLabel card [PayCardCost iid card (defaultWindows iid)] | card <- playable]
      pure e
    _ -> FriendsInLowPlaces . (`with` meta) <$> liftRunMessage msg attrs
