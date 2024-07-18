module Arkham.Event.Cards.EverVigilant4 (everVigilant4, EverVigilant4 (..)) where

import Arkham.Card
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Window

newtype EverVigilant4 = EverVigilant4 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant4 :: EventCard EverVigilant4
everVigilant4 = event EverVigilant4 Cards.everVigilant4

instance HasModifiersFor EverVigilant4 where
  getModifiersFor (InvestigatorTarget iid) (EverVigilant4 attrs) | iid == attrs.owner = do
    modified attrs [ReduceCostOf AnyCard 1]
  getModifiersFor _ _ = pure []

instance RunMessage EverVigilant4 where
  runMessage msg e@(EverVigilant4 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      doStep 4 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid eid) | eid == toId attrs && n > 0 -> do
      iids <- select $ affectsOthers $ colocatedWith iid
      hasPlayable <- flip filterM iids \iid' -> do
        cards <- fieldMap InvestigatorHand (filter (`cardMatch` CardWithType AssetType)) iid'
        anyM (getIsPlayable iid' GameSource (UnpaidCost NoAction) (defaultWindows iid')) cards

      when (notNull hasPlayable) do
        chooseOne iid $ Label "Do not play asset" []
          : [ targetLabel iid' [HandleTargetChoice iid (toSource attrs) (toTarget iid'), DoStep (n - 1) msg']
            | iid' <- hasPlayable
            ]
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      cards <- fieldMap InvestigatorHand (filter (`cardMatch` CardWithType AssetType)) iid
      playableCards <-
        filterM (getIsPlayable iid GameSource (UnpaidCost NoAction) (defaultWindows iid)) cards
      when (notNull playableCards)
        $ chooseOne
          iid
          [targetLabel (toCardId c) [PayCardCost iid c (defaultWindows iid)] | c <- playableCards]
      pure e
    _ -> EverVigilant4 <$> liftRunMessage msg attrs
