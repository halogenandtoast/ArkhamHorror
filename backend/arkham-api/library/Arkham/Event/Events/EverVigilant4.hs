module Arkham.Event.Events.EverVigilant4 (everVigilant4, EverVigilant4 (..)) where

import Arkham.Card
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getAsIfInHandCards, getIsPlayable)
import Arkham.Helpers.Modifiers (ModifierType (..), toModifiers, withModifiers)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Window

newtype EverVigilant4 = EverVigilant4 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant4 :: EventCard EverVigilant4
everVigilant4 = event EverVigilant4 Cards.everVigilant4

instance RunMessage EverVigilant4 where
  runMessage msg e@(EverVigilant4 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      doStep 4 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid eid) | eid == toId attrs && n > 0 -> do
      iids <- select $ affectsOthers $ colocatedWith iid
      hasPlayable <- flip filterM iids \iid' -> do
        withModifiers iid' (toModifiers attrs [ReduceCostOf AnyCard 1]) $ do
          cards <- fieldMap InvestigatorHand (filterCards $ card_ #asset) iid'
          asIfCards <- filterCards (card_ #asset) <$> getAsIfInHandCards iid
          anyM
            (getIsPlayable iid' GameSource (UnpaidCost NoAction) (defaultWindows iid'))
            (cards <> asIfCards)

      when (notNull hasPlayable) do
        chooseOne iid $ Label "Do not play asset" []
          : [ targetLabel iid' [HandleTargetChoice iid (toSource attrs) (toTarget iid'), DoStep (n - 1) msg']
            | iid' <- hasPlayable
            ]
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      cards <- fieldMap InvestigatorHand (filterCards (card_ #asset)) iid
      asIfCards <- filterCards (card_ #asset) <$> getAsIfInHandCards iid
      playableCards <- withModifiers iid (toModifiers attrs [ReduceCostOf AnyCard 1]) $ do
        filterM
          (getIsPlayable iid GameSource (UnpaidCost NoAction) (defaultWindows iid))
          (cards <> asIfCards)
      when (notNull playableCards) do
        chooseOneM iid do
          targets playableCards \c -> do
            costModifier attrs c (ReduceCostOf AnyCard 1)
            push $ PayCardCost iid c (defaultWindows iid)
      pure e
    _ -> EverVigilant4 <$> liftRunMessage msg attrs
