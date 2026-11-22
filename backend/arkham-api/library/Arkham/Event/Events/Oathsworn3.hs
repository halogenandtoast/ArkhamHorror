module Arkham.Event.Events.Oathsworn3 (oathsworn3) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Classes.HasQueue (HasQueue, overMessagesM)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (AssetDefeated)
import Arkham.Event.Import.Lifted qualified as Msg
import Arkham.Helpers.Dreaming (canCreateDreamingCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message (rewriteQueuedM)
import Arkham.Message.Lifted.Upgrade
import Arkham.Projection
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype Oathsworn3 = Oathsworn3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oathsworn3 :: EventCard Oathsworn3
oathsworn3 = event Oathsworn3 Cards.oathsworn3

instance HasModifiersFor Oathsworn3 where
  getModifiersFor (Oathsworn3 a) =
    for_ a.attachedTo.asset \aid -> modified_ a aid [HealthModifier 1, SanityModifier 1]

{- | The flip is the whole of the effect, so at the dreaming-card control limit
the reaction is not offered at all: the asset is defeated normally and Oathsworn
is discarded with it. The zone that has to have room is the ASSET CONTROLLER's,
who need not be the investigator holding the reaction.
-}
instance HasAbilities Oathsworn3 where
  getAbilities (Oathsworn3 a) = case a.placement of
    AttachedToAsset aid _ ->
      [ restricted a 1 (ControlsThis <> CanCreateDreamingCard (ControlsAsset $ AssetWithId aid))
          $ freeReaction (AssetDefeated #when ByAny (AssetWithId aid))
      ]
    _ -> []

instance RunMessage Oathsworn3 where
  runMessage msg e@(Oathsworn3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <-
        getUpgradeTargets iid
          $ AllyAsset
          <> AssetControlledBy (affectsOthersKnown iid $ colocatedWith iid)
      chooseTargetM iid assets \aid -> place attrs $ AttachedToAsset aid Nothing
      pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo.asset \aid -> do
        controller <- fieldMap AssetController (fromMaybe attrs.owner) aid
        -- re-read the cap, so the defeat is never replaced by a flip that then
        -- does nothing and leaves the damage sitting on the asset
        whenM (canCreateDreamingCard controller) do
          lift $ cancelAssetDefeat aid
          toDiscard (attrs.ability 1) attrs
          push $ FlipAssetFacedown controller aid 1
      pure e
    _ -> Oathsworn3 <$> liftRunMessage msg attrs

{- | "Discard Oathsworn instead": the defeat is replaced, so nothing past the
@#when@ window this resolves in may run -- neither the body that discards the
asset nor the @#after@ defeat windows a card like Decorated Skull reads.

'rewriteQueuedM' rather than a flat filter, for the reason 'insteadOfDamage'
gives: by the time a @When@ window's responder runs, the effect the window stands
in front of is already wrapped in 'MoveWithSkillTest', which no flat scan sees.
-}
cancelAssetDefeat :: HasQueue Message m => AssetId -> m ()
cancelAssetDefeat aid = overMessagesM $ rewriteQueuedM \case
  When (Msg.AssetDefeated _ aid') | aid' == aid -> pure []
  Do (Msg.AssetDefeated _ aid') | aid' == aid -> pure []
  After (Msg.AssetDefeated _ aid') | aid' == aid -> pure []
  CheckWindows ws -> case filter notThisDefeat ws of
    [] -> pure []
    ws' -> pure [CheckWindows ws']
  other -> pure [other]
 where
  notThisDefeat = \case
    (windowType -> Window.AssetDefeated aid' _) -> aid' /= aid
    _ -> True
