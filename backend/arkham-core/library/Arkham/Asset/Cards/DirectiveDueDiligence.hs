module Arkham.Asset.Cards.DirectiveDueDiligence (
  directiveDueDiligence,
  DirectiveDueDiligence (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Meta.RolandBanksParallel
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype DirectiveDueDiligence = DirectiveDueDiligence AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveDueDiligence :: AssetCard DirectiveDueDiligence
directiveDueDiligence = asset DirectiveDueDiligence Cards.directiveDueDiligence

instance HasModifiersFor DirectiveDueDiligence where
  getModifiersFor (InvestigatorTarget iid) (DirectiveDueDiligence a) = do
    maybeModified a do
      guard $ not a.flipped
      guard $ a.controller == Just iid
      meta <- lift $ fieldMap InvestigatorMeta (toResultDefault defaultMeta) iid
      guard $ dueDiligence meta >= 2
      guard $ "dueDiligence" `notElem` ignoredDirectives meta
      pure [CannotFight AnyEnemy]
  getModifiersFor _ _ = pure []

instance HasAbilities DirectiveDueDiligence where
  getAbilities (DirectiveDueDiligence a) =
    [ controlledAbility
      a
      1
      (DuringSkillTest (oneOf [#investigating, #evading, #parleying]) <> exists (EnemyIsEngagedWith You))
      $ FastAbility (exhaust a)
    | not a.flipped
    ]

instance RunMessage DirectiveDueDiligence where
  runMessage msg a@(DirectiveDueDiligence attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveDueDiligence $ attrs & flippedL .~ True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        enemies <- selectCount $ enemyEngagedWith iid
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue $ 2 * enemies)
      pure a
    _ -> DirectiveDueDiligence <$> liftRunMessage msg attrs
