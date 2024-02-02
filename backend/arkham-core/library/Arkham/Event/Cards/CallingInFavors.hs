module Arkham.Event.Cards.CallingInFavors (
  callingInFavors,
  CallingInFavors (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (PlayCard)
import Arkham.Projection

newtype CallingInFavors = CallingInFavors EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

callingInFavors :: EventCard CallingInFavors
callingInFavors = event CallingInFavors Cards.callingInFavors

instance RunMessage CallingInFavors where
  runMessage msg e@(CallingInFavors attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      allies <- selectList $ #ally <> assetControlledBy iid
      targetsWithCosts <- for allies \ally -> do
        cardDef <- field AssetCardDef ally
        pure (AssetTarget ally, maybe 0 toPrintedCost $ cdCost cardDef)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
            target
            [ ReturnToHand iid target
            , createCardEffect Cards.callingInFavors (Just $ EffectInt cost) attrs iid
            , search iid attrs iid [fromTopOfDeck 9] #ally (PlayFound iid 1)
            ]
          | (target, cost) <- targetsWithCosts
          ]
      pure e
    _ -> CallingInFavors <$> runMessage msg attrs
