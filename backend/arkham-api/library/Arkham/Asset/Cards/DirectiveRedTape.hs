module Arkham.Asset.Cards.DirectiveRedTape (
  directiveRedTape,
  DirectiveRedTape (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Investigator.Meta.RolandBanksParallel
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype DirectiveRedTape = DirectiveRedTape AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveRedTape :: AssetCard DirectiveRedTape
directiveRedTape = asset DirectiveRedTape Cards.directiveRedTape

instance HasModifiersFor DirectiveRedTape where
  getModifiersFor (InvestigatorTarget iid) (DirectiveRedTape a) = do
    maybeModified a do
      guard $ not a.flipped
      guard $ a.controller == Just iid
      meta <- lift $ fieldMap InvestigatorMeta (toResultDefault defaultMeta) iid
      pure
        $ [CanBecomeFast $ #event <> oneOf [#insight, #tactic] | a.ready]
        <> [CannotPlay AnyCard | redTape meta >= 2 && "redTape" `notElem` ignoredDirectives meta]
  getModifiersFor _ _ = pure []

instance HasAbilities DirectiveRedTape where
  getAbilities (DirectiveRedTape a) =
    [ restrictedAbility a 1 ControlsThis
      $ ReactionAbility
        (PlayCard #when You (basic $ #event <> oneOf [#insight, #tactic]))
        (exhaust a)
    | not a.flipped
    ]

instance RunMessage DirectiveRedTape where
  runMessage msg a@(DirectiveRedTape attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveRedTape $ attrs & flippedL .~ True
    UseCardAbility _ (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      costModifier (attrs.ability 1) card (BecomesFast FastPlayerWindow)
      pure a
    _ -> DirectiveRedTape <$> liftRunMessage msg attrs
