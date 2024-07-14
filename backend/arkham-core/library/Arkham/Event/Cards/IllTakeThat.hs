module Arkham.Event.Cards.IllTakeThat (illTakeThat, IllTakeThat (..)) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Window (getPassedBy)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Illicit))
import Arkham.Window (defaultWindows)

newtype IllTakeThat = IllTakeThat EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illTakeThat :: EventCard IllTakeThat
illTakeThat = event IllTakeThat Cards.illTakeThat

instance HasModifiersFor IllTakeThat where
  getModifiersFor target (IllTakeThat attrs) | attrs.attachedTo == Just target = do
    modified attrs [AddTrait Illicit]
  getModifiersFor _ _ = pure []

instance RunMessage IllTakeThat where
  runMessage msg e@(IllTakeThat attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let n = getPassedBy attrs.windows
      items <-
        select $ PlayableCardWithCostReduction NoAction n $ inHandOf iid <> basic (#item <> #asset)
      focusCards items \unfocus -> do
        chooseOne
          iid
          [ targetLabel
            item
            [ unfocus
            , Msg.costModifier attrs iid (ReduceCostOf (CardWithId $ toCardId item) 3)
            , PayCardCost iid item (defaultWindows iid)
            , HandleTargetChoice iid (toSource attrs) (CardIdTarget $ toCardId item)
            ]
          | item <- items
          ]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      selectOne (AssetWithCardId cid) >>= traverse_ \aid -> do
        push $ PlaceEvent iid attrs.id (AttachedToAsset aid Nothing)
      pure e
    _ -> IllTakeThat <$> liftRunMessage msg attrs
