module Arkham.Event.Events.Recharge2 (recharge2, Recharge2 (..)) where

import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Id
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype Meta = Meta {chosenAsset :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Recharge2 = Recharge2 (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recharge2 :: EventCard Recharge2
recharge2 = event (Recharge2 . (`With` Meta Nothing)) Cards.recharge2

instance RunMessage Recharge2 where
  runMessage msg e@(Recharge2 (attrs `With` meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs
        $ AssetControlledBy (affectsOthers $ colocatedWith iid)
        <> oneOf [#spell, #relic]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      revealChaosTokens attrs iid 1
      pure $ Recharge2 $ attrs `with` Meta (Just aid)
    RequestedChaosTokens (isSource attrs -> True) _ tokens -> do
      for_ (chosenAsset meta) \aid -> do
        let faces = [Skull, Cultist, Tablet, ElderThing, AutoFail]
        if any ((`elem` faces) . chaosTokenFace) tokens
          then
            push
              $ If
                (Window.RevealChaosTokenEventEffect attrs.controller tokens attrs.id)
                [Msg.toDiscardBy attrs.controller attrs aid]
          else push (AddUses (toSource attrs) aid Charge 3)
      resetChaosTokens attrs
      pure e
    _ -> Recharge2 . (`with` meta) <$> liftRunMessage msg attrs
