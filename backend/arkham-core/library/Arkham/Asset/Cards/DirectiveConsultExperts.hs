module Arkham.Asset.Cards.DirectiveConsultExperts (
  directiveConsultExperts,
  DirectiveConsultExperts (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Investigator.Meta.RolandBanksParallel
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Slot

newtype DirectiveConsultExperts = DirectiveConsultExperts AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveConsultExperts :: AssetCard DirectiveConsultExperts
directiveConsultExperts = asset DirectiveConsultExperts Cards.directiveConsultExperts

instance HasModifiersFor DirectiveConsultExperts where
  getModifiersFor (AssetTarget aid) (DirectiveConsultExperts a) = do
    maybeModified a do
      guard $ not a.flipped
      controller <- hoistMaybe a.controller
      liftGuardM $ aid <=~> asset_ #ally
      meta <- lift $ fieldMap InvestigatorMeta (toResultDefault defaultMeta) controller
      guard $ "consultExperts" `notElem` ignoredDirectives meta
      pure [CannotAssignDamage controller]
  getModifiersFor _ _ = pure []

instance RunMessage DirectiveConsultExperts where
  runMessage msg (DirectiveConsultExperts attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid AllySlot $ Slot (toSource attrs) []
      DirectiveConsultExperts <$> runMessage msg attrs
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveConsultExperts $ attrs & flippedL .~ True
    _ -> DirectiveConsultExperts <$> liftRunMessage msg attrs
