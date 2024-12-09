module Arkham.Event.Events.FriendsInLowPlaces (friendsInLowPlaces, FriendsInLowPlaces (..)) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayable)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Cost
import Arkham.Helpers.Customization
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
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
  getModifiersFor (FriendsInLowPlaces (With a _)) =
    modifySelfWhen a (a `hasCustomization` Prompt) [BecomesFast FastPlayerWindow]

instance RunMessage FriendsInLowPlaces where
  runMessage msg e@(FriendsInLowPlaces (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      ts <- select $ affectsOthers $ colocatedWith iid
      if notNull ts && attrs `hasCustomization` Helpful
        then chooseOrRunOne iid $ targetLabels ts $ only . handleTargetChoice iid attrs
        else push $ HandleTargetChoice iid (toSource attrs) (toTarget iid)
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      let traits = [t | ChosenTrait t <- concatMap snd (toList attrs.customizations)]
      let lookSources = [fromTopOfDeck $ if attrs `hasCustomization` Experienced then 9 else 6]
      lookAt iid' attrs iid' lookSources (basic $ mapOneOf CardWithTrait traits) (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) x cards -> do
      when (null cards) $ chooseOne iid [Label "No Cards Founds" []]
      when (notNull cards) do
        n <- getSpendableResources iid
        let traits = [t | ChosenTrait t <- concatMap snd (toList attrs.customizations)]
        let hasBothTraits = filterCards (foldMap CardWithTrait traits) cards
        if attrs `hasCustomization` Versatile && notNull hasBothTraits && n > 0
          then do
            chooseOneM iid do
              labeled "Do not add a card to your hand for free (Versatile)" $ doStep 0 msg
              for_ (eachWithRest hasBothTraits) \(card, cards') -> do
                targeting card do
                  when (attrs `hasCustomization` Bolstering) $ phaseModifier attrs card (AddSkillIcons [#wild])
                  addToHand iid (only card)
                  handleTarget iid attrs card
                  doStep 0 $ SearchFound iid (toTarget attrs) x cards'
          else doStep 0 msg
      when (attrs `hasCustomization` Clever) do
        chooseOneM iid do
          labeled "Shuffle Cards Back In" nothing
          labeled "Place on the top of your deck, in any order" do
            push $ UpdateSearchReturnStrategy iid FromDeck PutBackInAnyOrder
      pushWhen (attrs `hasCustomization` Swift) $ Do msg
      pure e
    DoStep _ (SearchFound iid (isTarget attrs -> True) x cards) | notNull cards -> do
      n <- getSpendableResources iid
      when (n > 0) do
        chooseOneM iid do
          labeled "Do not spend 1 resource to add a card to your hand" nothing
          for_ (eachWithRest cards) \(card, cards') -> do
            targeting card do
              push $ SpendResources iid 1
              when (attrs `hasCustomization` Bolstering) $ phaseModifier attrs card (AddSkillIcons [#wild])
              addToHand iid (only card)
              handleTarget iid attrs card
              doStep 0 $ SearchFound iid (toTarget attrs) x cards'
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (CardIdTarget cid) -> do
      pure . FriendsInLowPlaces $ attrs `with` Metadata (cid : chosenCards meta)
    Do (SearchFound iid (isTarget attrs -> True) _ _) -> do
      when (notNull $ chosenCards meta) do
        cards <- traverse getCard (chosenCards meta)
        playable <- filterM (getIsPlayable iid attrs (UnpaidCost NoAction) (defaultWindows iid)) cards
        when (notNull playable) do
          chooseOneM iid do
            labeled "Do no play cards (Swift)" nothing
            targets playable \card -> push $ PayCardCost iid card (defaultWindows iid)
      pure e
    _ -> FriendsInLowPlaces . (`with` meta) <$> liftRunMessage msg attrs
