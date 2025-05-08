module Arkham.Location.Cards.SeaOfSkulls (seaOfSkulls) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Location)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype SeaOfSkulls = SeaOfSkulls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfSkulls :: LocationCard SeaOfSkulls
seaOfSkulls =
  locationWith SeaOfSkulls Cards.seaOfSkulls 4 (PerPlayer 1)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities SeaOfSkulls where
  getAbilities (SeaOfSkulls a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnEnds #after You
      , restricted
          a
          2
          ( exists
              $ FarthestLocationFromLocation a.id Anywhere
              <> mapOneOf (`LocationWithSpaceInDirection` Anywhere) [Above, Below, RightOf]
          )
          $ forced
          $ RevealLocation #when Anyone (be a)
      ]

instance RunMessage SeaOfSkulls where
  runMessage msg l@(SeaOfSkulls attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasCardsInHand <- selectAny $ inHandOf NotForPlay iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDirectHorror" $ directHorror iid (attrs.ability 1) 1
        when hasCardsInHand do
          countVar 3 $ labeled' "discardCardsFromHand" $ chooseAndDiscardCards iid (attrs.ability 1) 3
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [_] ->
          chooseSelectM
            iid
            ( FarthestLocationFromLocation attrs.id Anywhere
                <> mapOneOf (`LocationWithSpaceInDirection` Anywhere) [Above, Below, RightOf]
            )
            (`forTarget` msg)
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    ForTarget (LocationTarget lid) (DrewCards iid drewCards) | maybe False (isTarget attrs) drewCards.target -> do
      attrs' <- getAttrs @Location lid
      case drewCards.cards of
        [card] -> chooseOrRunOneM iid $ scenarioI18n do
          whenM (directionEmpty attrs' Above) $ labeled' "above" do
            lid' <- placeAtDirection Above attrs' card
            placeTokens (attrs.ability 1) lid' #horror 1
            push $ AddDirectConnection attrs.id lid'
          whenM (directionEmpty attrs' Below) $ labeled' "below" do
            lid' <- placeAtDirection Below attrs' card
            placeTokens (attrs.ability 1) lid' #horror 1
            push $ AddDirectConnection attrs.id lid'
          whenM (directionEmpty attrs' RightOf) $ labeled' "right" do
            lid' <- placeAtDirection RightOf attrs' card
            placeTokens (attrs.ability 1) lid' #horror 1
            push $ AddDirectConnection attrs.id lid'
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> SeaOfSkulls <$> liftRunMessage msg attrs
