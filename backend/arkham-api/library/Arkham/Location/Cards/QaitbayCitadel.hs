module Arkham.Location.Cards.QaitbayCitadel (qaitbayCitadel) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype QaitbayCitadel = QaitbayCitadel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

qaitbayCitadel :: LocationCard QaitbayCitadel
qaitbayCitadel = symbolLabel $ location QaitbayCitadel Cards.qaitbayCitadel 3 (PerPlayer 1)

instance HasAbilities QaitbayCitadel where
  getAbilities (QaitbayCitadel a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage QaitbayCitadel where
  runMessage msg l@(QaitbayCitadel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifiers (attrs.ability 1) iid [CannotDiscoverCluesAt (be attrs), noExposeAt attrs]
      pure l
    _ -> QaitbayCitadel <$> liftRunMessage msg attrs
