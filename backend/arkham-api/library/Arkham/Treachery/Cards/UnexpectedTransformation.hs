module Arkham.Treachery.Cards.UnexpectedTransformation (unexpectedTransformation) where

import Arkham.Asset.Types (Field (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnexpectedTransformation = UnexpectedTransformation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedTransformation :: TreacheryCard UnexpectedTransformation
unexpectedTransformation = treachery UnexpectedTransformation Cards.unexpectedTransformation

instance RunMessage UnexpectedTransformation where
  runMessage msg t@(UnexpectedTransformation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ AssetWithHighestPrintedCost $ assetControlledBy iid
      chooseTargetM iid assets $ handleTarget iid attrs
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget asset) -> do
      cost <- field AssetCost asset
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed cost)
      pure $ UnexpectedTransformation $ setMeta asset attrs
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      for_ (maybeResult @AssetId attrs.meta) \asset -> do
        card <- fetchCard asset
        let horror = count (`elem` [#willpower, #intellect]) card.icons
        let damage = count (`elem` [#combat, #agility]) card.icons
        chooseOneM iid $ withI18n do
          labeled' "discardThatAsset" $ toDiscardBy iid attrs asset
          numberVar "horror" horror
            $ numberVar "damage" damage
            $ labeled' "takeHorrorAndDamage"
            $ assignDamageAndHorror iid (attrs.ability 1) damage horror

      pure t
    _ -> UnexpectedTransformation <$> liftRunMessage msg attrs
