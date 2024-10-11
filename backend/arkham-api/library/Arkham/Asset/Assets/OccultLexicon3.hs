module Arkham.Asset.Assets.OccultLexicon3 (occultLexicon3, OccultLexicon3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (getCanShuffleDeck, searchBonded)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Strategy

newtype OccultLexicon3 = OccultLexicon3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultLexicon3 :: AssetCard OccultLexicon3
occultLexicon3 = asset OccultLexicon3 Cards.occultLexicon3

instance HasAbilities OccultLexicon3 where
  getAbilities (OccultLexicon3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ Matcher.PlayCard #when You (basic $ cardIs Events.bloodRite)
    ]

instance RunMessage OccultLexicon3 where
  runMessage msg a@(OccultLexicon3 attrs) = runQueueT $ case msg of
    InvestigatorPlayAsset iid aid | aid == attrs.id -> do
      bonded <- take 3 <$> searchBonded iid Events.bloodRite
      case bonded of
        [] -> pure ()
        (handBloodRite : deckBloodRites) -> do
          addToHand iid (only handBloodRite)
          whenM (getCanShuffleDeck iid) $ shuffleCardsIntoDeck iid deckBloodRites
      OccultLexicon3 <$> liftRunMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      chooseOneM iid do
        labeled "Change each \"2\" to a \"3\"" do
          eventModifier (attrs.ability 1) card (MetaModifier $ object ["use3" .= True])
        labeled "Shuffle it into your deck instead of discarding it" do
          eventModifier (attrs.ability 1) card (SetAfterPlay ShuffleThisBackIntoDeck)
      pure a
    _ -> OccultLexicon3 <$> liftRunMessage msg attrs
