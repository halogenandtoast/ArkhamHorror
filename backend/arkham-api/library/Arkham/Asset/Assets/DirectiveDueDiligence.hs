module Arkham.Asset.Assets.DirectiveDueDiligence (directiveDueDiligence) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (choose)
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Control.Lens (non)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Bool, _Integer)

newtype DirectiveDueDiligence = DirectiveDueDiligence AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveDueDiligence :: AssetCard DirectiveDueDiligence
directiveDueDiligence = asset DirectiveDueDiligence Cards.directiveDueDiligence

instance HasModifiersFor DirectiveDueDiligence where
  getModifiersFor (DirectiveDueDiligence a) = for_ a.controller \iid -> do
    unless a.flipped do
      let ignored = fromMaybe False $ a ^? metaMapL . ix "ignore_regulation" . _Bool
      let dueDiligence = fromMaybe 0 $ a ^? metaMapL . ix "due_diligence" . _Integer
      modifiedWhen_ a (dueDiligence >= 2 && not ignored) iid [CannotFight AnyEnemy]

instance HasAbilities DirectiveDueDiligence where
  getAbilities (DirectiveDueDiligence a) =
    [ controlled
        a
        1
        (DuringSkillTest (oneOf [#investigating, #evading, #parleying]) <> exists (EnemyIsEngagedWith You))
        $ FastAbility (exhaust a)
    | not a.flipped
    ]

instance RunMessage DirectiveDueDiligence where
  runMessage msg a@(DirectiveDueDiligence attrs) = runQueueT $ case msg of
    Do BeginRound ->
      pure
        $ DirectiveDueDiligence
        $ attrs
        & (metaMapL %~ KeyMap.delete "ignore_regulation" . KeyMap.delete "due_diligence")
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveDueDiligence $ attrs & flippedL .~ True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        enemies <- selectCount $ enemyEngagedWith iid
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue $ 2 * enemies)
      pure a
    FightEnemy _ choose | Just choose.investigator == attrs.controller -> do
      pure
        $ DirectiveDueDiligence
        $ attrs
        & metaMapL
        . at "due_diligence"
        . non (Number 0)
        . _Integer
        +~ 1
    _ -> DirectiveDueDiligence <$> liftRunMessage msg attrs
