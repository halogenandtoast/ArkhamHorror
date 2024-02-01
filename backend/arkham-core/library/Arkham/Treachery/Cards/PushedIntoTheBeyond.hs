module Arkham.Treachery.Cards.PushedIntoTheBeyond (
  PushedIntoTheBeyond (..),
  pushedIntoTheBeyond,
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PushedIntoTheBeyond = PushedIntoTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

pushedIntoTheBeyond :: TreacheryCard PushedIntoTheBeyond
pushedIntoTheBeyond = treachery PushedIntoTheBeyond Cards.pushedIntoTheBeyond

instance RunMessage PushedIntoTheBeyond where
  runMessage msg t@(PushedIntoTheBeyond attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <-
        selectWithField AssetCardCode $ AssetControlledBy You <> AssetNonStory
      player <- getPlayer iid
      when (notNull targets)
        $ push
        $ chooseOne
          player
          [ targetLabel
            aid
            [ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (AssetTarget aid)
            , CreateEffect
                (CardCode "02100")
                (Just (EffectCardCodes [cardCode]))
                (toSource attrs)
                (InvestigatorTarget iid)
            ]
          | (aid, cardCode) <- targets
          ]
      pure t
    _ -> PushedIntoTheBeyond <$> runMessage msg attrs
