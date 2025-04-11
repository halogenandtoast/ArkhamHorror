module Arkham.Location.Cards.ReturnToCloverClubLounge (returnToCloverClubLounge) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype ReturnToCloverClubLounge = ReturnToCloverClubLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCloverClubLounge :: LocationCard ReturnToCloverClubLounge
returnToCloverClubLounge = symbolLabel $ location ReturnToCloverClubLounge Cards.returnToCloverClubLounge 4 (Static 0)

instance HasAbilities ReturnToCloverClubLounge where
  getAbilities (ReturnToCloverClubLounge a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , playerLimit PerRound $ restricted a 2 (Here <> OnAct 1 <> youExist can.manipulate.deck) actionAbility
      ]

instance RunMessage ReturnToCloverClubLounge where
  runMessage msg l@(ReturnToCloverClubLounge attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      whenM (selectNone $ locationIs Cards.cloverClubStage) do
        placeSetAsideLocation_ Cards.cloverClubStage
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      lookAt iid (attrs.ability 2) iid [fromTopOfDeck 1] #any (defer attrs IsNotDraw)
      pure l
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      case cards of
        [card] ->
          if card `cardMatch` card_ (#ally <> #asset)
            then chooseOneM iid do
              labeled "Put it into play" $ putCardIntoPlay iid card
              labeled "Do not put it into play" nothing
            else whenM (can.draw.cards iid) $ drawCard iid card
        xs ->
          chooseOneAtATimeM iid do
            targets xs \card ->
              if card `cardMatch` card_ (#ally <> #asset)
                then chooseOneM iid do
                  labeled "Put it into play" $ putCardIntoPlay iid card
                  labeled "Do not put it into play" nothing
                else whenM (can.draw.cards iid) $ drawCard iid card
      pure l
    _ -> ReturnToCloverClubLounge <$> liftRunMessage msg attrs
