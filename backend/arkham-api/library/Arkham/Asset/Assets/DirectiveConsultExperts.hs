module Arkham.Asset.Assets.DirectiveConsultExperts (
  directiveConsultExperts,
  DirectiveConsultExperts (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
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
  getModifiersFor (DirectiveConsultExperts a) = case a.controller of
    Just iid | not a.flipped -> do
      meta <- fieldMap InvestigatorMeta (toResultDefault defaultMeta) iid
      modifySelectWhen
        a
        ("consultExperts" `notElem` ignoredDirectives meta)
        (asset_ #ally)
        [CannotAssignDamage iid]
    _ -> pure mempty

instance RunMessage DirectiveConsultExperts where
  runMessage msg (DirectiveConsultExperts attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid AllySlot $ Slot (toSource attrs) []
      DirectiveConsultExperts <$> liftRunMessage msg attrs
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveConsultExperts $ attrs & flippedL .~ True
    _ -> DirectiveConsultExperts <$> liftRunMessage msg attrs
