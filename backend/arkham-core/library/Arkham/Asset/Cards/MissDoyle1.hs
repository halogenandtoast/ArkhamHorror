module Arkham.Asset.Cards.MissDoyle1 (
  missDoyle1,
  MissDoyle1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck
import {-# SOURCE #-} Arkham.GameEnv (findCard)
import Arkham.Helpers.Investigator hiding (findCard)
import Arkham.Matcher

newtype MissDoyle1 = MissDoyle1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

missDoyle1 :: AssetCard MissDoyle1
missDoyle1 = ally MissDoyle1 Cards.missDoyle1 (2, 2)

{- Forced: After Miss Doyle enters play: Search your bonded cards for Hope, Zeal, and Augur. Randomly choose 1 to put into play and shuffle the other 2 into your deck. When Miss Doyle leaves play, find each of those assets (even if they are out of play) and remove them from the game. -}

instance HasAbilities MissDoyle1 where
  getAbilities (MissDoyle1 a) =
    [ restrictedAbility a 1 ControlsThis $ ForcedAbility $ AssetEntersPlay #when $ AssetWithId (toId a)
    , mkAbility a 2
        $ SilentForcedAbility
        $ AssetLeavesPlay #when
        $ AssetWithId
        $ toId a
    ]

instance RunMessage MissDoyle1 where
  runMessage msg a@(MissDoyle1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hope <- fromJustNote "must be" . listToMaybe <$> searchBonded iid Cards.hope
      augur <- fromJustNote "must be" . listToMaybe <$> searchBonded iid Cards.augur
      zeal <- fromJustNote "must be" . listToMaybe <$> searchBonded iid Cards.zeal
      let cats = [hope, augur, zeal]

      player <- getPlayer iid

      push
        $ questionLabel "Choose one to put into play, the others will be shuffled into your deck" player
        $ ChooseOne
          [ CardLabel
            (toCardCode playCat)
            [PutCardIntoPlay iid playCat Nothing [], ShuffleCardsIntoDeck (InvestigatorDeck iid) deckCats]
          | (playCat, deckCats) <- eachWithRest cats
          ]

      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      mhope <- findCard (`cardMatch` (cardIs Cards.hope <> CardOwnedBy iid))
      mzeal <- findCard (`cardMatch` (cardIs Cards.zeal <> CardOwnedBy iid))
      maugur <- findCard (`cardMatch` (cardIs Cards.augur <> CardOwnedBy iid))

      for_ (catMaybes $ traceShowId [mhope, mzeal, maugur]) $ \cat ->
        push $ PlaceInBonded iid cat

      pure a
    _ -> MissDoyle1 <$> runMessage msg attrs
