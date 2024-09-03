module Arkham.Treachery.Cards.BuriedSecrets (buriedSecrets, BuriedSecrets (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Capability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BuriedSecrets = BuriedSecrets TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buriedSecrets :: TreacheryCard BuriedSecrets
buriedSecrets = treachery BuriedSecrets Cards.buriedSecrets

instance HasAbilities BuriedSecrets where
  getAbilities (BuriedSecrets a) = [restrictedAbility a 1 OnSameLocation investigateAction_]

instance HasModifiersFor BuriedSecrets where
  getModifiersFor (InvestigatorTarget iid) (BuriedSecrets a) | a `on` iid = do
    canInvestigate <- selectAny $ locationWithInvestigator iid <> InvestigatableLocation
    pure $ toModifiers a [CannotMoveExceptByScenarioCardEffects | canInvestigate]
  getModifiersFor _ _ = pure []

instance RunMessage BuriedSecrets where
  runMessage msg t@(BuriedSecrets attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 1) <&> setTarget attrs
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    Failed (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      mOwner <- field TreacheryOwner attrs.id
      for_ mOwner \owner -> do
        canManipulateDeck <- can.manipulate.deck owner
        when canManipulateDeck $ do
          chooseOne
            iid
            [ Label
                "Take 2 horror to shuffle into your deck"
                [Msg.assignHorror iid (attrs.ability 1) 2, Msg.shuffleIntoDeck iid attrs]
            , Label "Do Nothing" []
            ]
      pure t
    _ -> BuriedSecrets <$> liftRunMessage msg attrs
