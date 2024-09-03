module Arkham.Asset.Cards.DirectiveSeekTheTruth (
  directiveSeekTheTruth,
  DirectiveSeekTheTruth (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Investigator.Meta.RolandBanksParallel
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype DirectiveSeekTheTruth = DirectiveSeekTheTruth AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveSeekTheTruth :: AssetCard DirectiveSeekTheTruth
directiveSeekTheTruth = asset DirectiveSeekTheTruth Cards.directiveSeekTheTruth

instance HasModifiersFor DirectiveSeekTheTruth where
  getModifiersFor (InvestigatorTarget iid) (DirectiveSeekTheTruth a) = do
    maybeModified a do
      guard $ not a.flipped
      guard $ a.controller == Just iid
      meta <- lift $ fieldMap InvestigatorMeta (toResultDefault defaultMeta) iid
      guard $ seekTheTruth meta
      guard $ "seekTheTruth" `notElem` ignoredDirectives meta
      liftGuardM $ selectAny $ locationWithInvestigator iid <> LocationWithAnyClues
      pure [CannotCommitCards AnyCard]
  getModifiersFor _ _ = pure []

instance HasAbilities DirectiveSeekTheTruth where
  getAbilities (DirectiveSeekTheTruth a) =
    [ controlledAbility a 1 (exists (EnemyAt YourLocation) <> can.draw.cards You)
      $ ReactionAbility
        (DiscoverClues #after You Anywhere AnyValue)
        (exhaust a)
    | not a.flipped
    ]

instance RunMessage DirectiveSeekTheTruth where
  runMessage msg a@(DirectiveSeekTheTruth attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveSeekTheTruth $ attrs & flippedL .~ True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> DirectiveSeekTheTruth <$> liftRunMessage msg attrs
